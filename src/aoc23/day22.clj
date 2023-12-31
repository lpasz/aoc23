(ns aoc23.day22
  "Sand Slabs"
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn parse-input [inp]
  (->> (re-seq #"(?m)(\d+),(\d+),(\d+)\~(\d+),(\d+),(\d+)" inp)
       (map rest)
       (map #(map parse-long %))))

(defn rangex [p1 p2]
  (range (min p1 p2) (inc (max p1 p2))))

(defn get-positions [[x1 y1 z1 x2 y2 z2]]
  (vec (for [x (rangex x1 x2)
             y (rangex y1 y2)
             z (rangex z1 z2)]
         [x y z])))

(defn step-down [[x1 y1 z1 x2 y2 z2]]
  [x1 y1 (dec z1) x2 y2 (dec z2)])

(defn floor? [[_ _ z1 _ _ z2]] (or (zero? z1) (zero? z2)))

(defn next-brick-pos [brick occupied]
  (let [brick-down (step-down brick)]
    (cond (floor? brick-down) brick
          (some occupied (get-positions brick-down)) brick
          :else (or (next-brick-pos brick-down occupied) brick-down))))

(defn brick-fall [bricks]
  (loop [bricks (sort-by #(nth % 2) bricks)
         bricks-after-falling []
         occupied {}]
    (if (empty? bricks)
      [bricks-after-falling occupied]
      (let [brick (first bricks)
            bricks (rest bricks)
            brick-next (next-brick-pos brick occupied)
            bricks-after-falling (conj bricks-after-falling brick-next)
            occupied (reduce #(assoc %1 %2 brick-next) occupied (get-positions brick-next))]
        (recur bricks bricks-after-falling occupied)))))

(defn brick-fall-with-id [bricks]
  (loop [bricks (sort-by #(nth (second %) 2) bricks)
         bricks-after-falling []
         occupied {}]
    (if (empty? bricks)
      bricks-after-falling
      (let [[id brick] (first bricks)
            bricks (rest bricks)
            brick-next (next-brick-pos brick occupied)
            bricks-after-falling (conj bricks-after-falling [id brick-next])
            occupied (reduce #(assoc %1 %2 brick-next) occupied (get-positions brick-next))]
        (recur bricks bricks-after-falling occupied)))))

(defn supports-and-supported-by [bricks-after-fall occupied]
  (->> bricks-after-fall
       (reduce (fn [acc brick]
                 (let [brick-pos (get-positions brick)
                       occupied-by-others (reduce dissoc occupied brick-pos)
                       brick-down-pos (get-positions (step-down brick))
                       supported-by-bricks (set (keep occupied-by-others brick-down-pos))]
                   (if (empty? supported-by-bricks)
                     acc
                     (assoc acc brick supported-by-bricks))))
               {})
       (c/then [supported-by-bricks]
               [(c/revert-map supported-by-bricks #{}) supported-by-bricks])))

(defn check-if-would-fall [brick support-bricks support-by-bricks]
  (->> (support-bricks brick)
       (map support-by-bricks)
       (filter #(= #{brick} %))
       (not-empty)))

(defn part1 [inp]
  (let [bricks (parse-input inp)
        [bricks-after-fall occupied] (brick-fall bricks)
        [support-bricks supporte-by-bricks] (supports-and-supported-by bricks-after-fall occupied)]
    (- (count bricks-after-fall)
       (->> bricks-after-fall
            (filter #(check-if-would-fall % support-bricks supporte-by-bricks))
            (count)))))

(defn part2 [inp]
  (let [bricks (parse-input inp)
        [bricks-after-fall occupied] (brick-fall bricks)
        [support-bricks supporte-by-bricks] (supports-and-supported-by bricks-after-fall occupied)]
    (->> bricks-after-fall
         (filter #(check-if-would-fall % support-bricks supporte-by-bricks))
         (map (fn [ubrick]
                (let [other-bricks (map-indexed (fn [id itm] [id itm]) (c/remove-first #{ubrick} bricks-after-fall))
                      other-bricks-set (set other-bricks)
                      other-bricks-after-fall  (brick-fall-with-id other-bricks)]
                  (->> other-bricks-after-fall
                       (c/reject other-bricks-set)
                       (count)))))
         (reduce +))))

(comment
  (assert (= 5 (part1 exp1-input)))
  (assert (= 393 (part1 part1-input)))
  (assert (= 7 (part2 exp1-input)))
  (assert (= 58440 (part2 part1-input)))
  ;;
  )
