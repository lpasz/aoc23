(ns aoc23.day24
  "Never Tell Me The Odds"
  (:require [core :as c]
            [clojure.string :as s]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn in-range [s f [x y]]
  (and (<= s x f) (<= s y f)))

(def part1-rng (partial in-range 200000000000000 400000000000000))
(def exp1-rng (partial in-range 7 27))

(defn after? [[x y vx vy]]
  (let [aftr #({-1 < 1 >} (compare % 0))]
    (fn [[nx ny]]
      (and ((aftr vx) nx x)
           ((aftr vy) ny y)))))

(defn next-point-xy [[x y vx vy]]
  [(+ x vx) (+ y vy)])

(defn compute-line [[x1 y1] [x2 y2]]
  (when-not (zero? (- x2 x1))
    (let [m (/ (- y2 y1)
               (- x2 x1))]
      {:slope  m
       :offset (- y1 (* m x1))})))

(defn parse-raw [inp]
  (->> (re-seq #"(?m)(.*)\,(.*)\,(.*)\@(.*)\,(.*)\,(.*)" inp)
       (mapcat rest)
       (map s/trim)
       (map  parse-long)
       (partition 6)))

(defn parse [inp]
  (->> (parse-raw inp)
       (map (fn [[x y z vx vy vz]]
              {:raw [x y z vx vy vz]
               :pt1 [x y]
               :pt2 (next-point-xy [x y vx vy])
               :after-xy? (after? [x y z vx vy])}))
       (map (fn [{:keys [pt1 pt2] :as m}] (merge m (compute-line pt1 pt2))))))


(defn intercept [line1 line2]
  (let [div (- (:slope  line2) (:slope  line1))]
    (if (zero? div)
      nil
      (let [x (/ (- (:offset line1) (:offset line2)) div)
            y (+ (* (:slope line1) x)
                 (:offset line1))]
        [x y]))))

(defn combinations [coll]
  (->> (for [h1 coll h2 coll :when (not= h1 h2)]
         #{h1 h2})
       (into #{})
       (map vec)))

(defn part1 [inp rng]
  (->> (parse inp)
       (combinations)
       (keep (fn [[line1 line2]]
               (when-let [intersect (intercept line1 line2)]
                 [(assoc line1 :intersect intersect)
                  (assoc line2 :intersect intersect)])))
       (filter (fn [[line1 line2]] (and ((:after? line1) (:intersect line1))
                                        ((:after? line2) (:intersect line2)))))
       (map first)
       (map :intersect)
       (filter rng)
       (count)))

(defn update-velocity [[x y vx vy] vel1 vel2]
  [x y (+ vx vel1) (+ vy vel2)])

(defn to-v3  [[x y z vx vy vz]] {:x x :y y :z z :vx vx :vy vy :vz vz})

(defn to-line [hailstone vx vy]
  (let [hailstone (update-velocity hailstone vx vy)
        next-hailstone (next-point-xy hailstone)]
    (compute-line hailstone next-hailstone)))

(defn all-hailstones-colide-at [hailstones vx vy]
  (loop [hailstones hailstones
         colide-at nil]
    (if (or (c/one? (count hailstones))
            (empty? hailstones))
      colide-at
      (let [h1-line (to-line (first hailstones) vx vy)
            h2-line (to-line (second hailstones) vx vy)
            hrest (rest (rest hailstones))]
        (when (and h1-line h2-line)
          (when-let [intercept (intercept h1-line h2-line)]
            (let [intercept-at (conj intercept vx vy)]
              (cond (nil? colide-at) (recur hrest (conj intercept vx vy))
                    (= intercept-at colide-at) (recur hrest intercept-at)))))))))

(defn relative-rock-finding [hailstones min-velocity max-velocity]
  (let [velocities (range min-velocity (inc max-velocity))]
    (->> (for [vx velocities
               vy velocities
               :when (or (not (zero? vx))
                         (not (zero? vy)))]
           (when-let [colide-at (all-hailstones-colide-at hailstones vx vy)]
             colide-at))
         (filter #(not (nil? %)))
         (first))))

(defn part2 [inp]
  (let [raw (->> (parse-raw inp))
        rawm (map to-v3 raw)
        min-velocity (->> [:vx :vy :vz] (map #(c/min-by % rawm)) (reduce min))
        max-velocity (->> [:vx :vy :vz] (map #(c/max-by % rawm)) (reduce max))
        max-rng (max (abs min-velocity) (abs max-velocity))
        raw-xy (map (fn [[x y _z vx vy _vz]] [x y vx vy]) raw)
        raw-xz (map (fn [[x _y z vx _vy vz]] [x z vx vz]) raw)
        [x y _ _] (relative-rock-finding raw-xy (- max-rng) max-rng)
        [_ z _ _] (relative-rock-finding raw-xz (- max-rng) max-rng)]
    (+ x y z)))

(comment
  (assert (= 2 (part1 exp1-input exp1-rng)))
  (assert (= 12740 (part1 part1-input part1-rng)))
  (assert (= 47 (part2 exp1-input))) 
  (assert (= 741991571910536 (part2 part1-input)))
  ;;
  )

