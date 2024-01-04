(ns aoc23.day24
  (:require [core :as c]
            [clojure.string :as s]))


(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn in-range [s f [x y]]
  (and (<= s x f) (<= s y f)))

(def part1-rng (partial in-range 200000000000000 400000000000000))
(def exp1-rng (partial in-range 7 27))

(defn after? [[x y _z vx vy _vz]]
  (let [aftr #({-1 < 1 >} (compare % 0))]
    (fn [[nx ny]]
      (and ((aftr vx) nx x)
           ((aftr vy) ny y)))))

(defn next-point [[x y _ vx vy _]]
  [(+ x vx) (+ y vy)])

(defn compute-line [pt1 pt2]
  (let [[x1 y1] pt1
        [x2 y2] pt2
        m (/ (- y2 y1) (- x2 x1))]
    {:slope  m
     :offset (- y1 (* m x1))}))

(def f (parse exp1-input))

((:after? (first f)) [14.333 15.333])
((:after? (second f)) [14.333 15.333])

(compare -1 0)

(and  (> 18 14.333) (> 19 15.333))

(defn parse [inp]
  (->> (re-seq #"(?m)(.*)\,(.*)\,(.*)\@(.*)\,(.*)\,(.*)" inp)
       (mapcat rest)
       (map s/trim)
       (map  parse-long)
       (partition 6)
       (map (fn [[x y z vx vy vz]]
              {:raw [x y z vx vy vz]
               :pt1 [x y]
               :pt2 (next-point [x y z vx vy vz])
               :after? (after? [x y z vx vy vz])}))
       (map (fn [{:keys [pt1 pt2] :as m}]
              (merge m (compute-line pt1 pt2))))))


(defn intercept [line1 line2]
  (let [div (- (:slope  line2) (:slope  line1))]
    (if (zero? div)
      nil
      (let [x (/ (- (:offset line1) (:offset line2)) (float div))
            y (+ (* (:slope line1) x)
                 (:offset line1))]
        [x y]))))

(defn part1 [inp rng]
  (let [exp1 (parse inp)]
    (->> (for [h1 exp1 h2 exp1 :when (not= h1 h2)]
           #{h1 h2})
         (into #{})
         (map vec)
         (keep (fn [[line1 line2]]
                 (if-let [intersect (intercept line1 line2)]
                   [(assoc line1 :intersect intersect)
                    (assoc line2 :intersect intersect)])))
         (filter (fn [[line1 line2]] (and ((:after? line1) (:intersect line1))
                                          ((:after? line2) (:intersect line2)))))
         (map first)
         (map :intersect)
         (filter rng)
         (count))))

(comment
  (assert (= 2 (part1 exp1-input exp1-rng)))
  (assert (= 12740 (part1 part1-input part1-rng)))
  ;;
  )