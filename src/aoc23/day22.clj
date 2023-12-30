(ns aoc23.day22
  "Sand Slabs"
  (:require [core :as c]
            [clojure.set :as set]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))
(def part1-other-input (c/get-input "part1-other.txt"))

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

(defn fallme [bricks]
  (loop [bricks bricks
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


;; (defn fall [bricks]
;;   (reduce (fn [[fallen occupied] brick]
;;             (loop [cur brick
;;                    nxt (step-down cur)]
;;               (if (and (> (nth nxt 2) 0)
;;                        (not (some #(get occupied %) (get-positions nxt))))
;;                 (recur nxt (step-down nxt))
;;                 [(conj fallen cur)
;;                  (reduce #(assoc %1 %2 cur) occupied (get-positions cur))])))
;;           [[] {}]
;;           bricks))

(defn supports-and-supported-by [fallen occupied]
  (->> fallen
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


;; (defn check-field [fallen occupied]
;;   (reduce (fn [[above below] brick]
;;             (let [curposs (get-positions brick)]
;;               (reduce (fn [[a b] pos]
;;                         (if (and (get occupied pos)
;;                                  (not (some #{pos} curposs)))
;;                           [(assoc a (get occupied pos)
;;                                   (conj (get a (get occupied pos) #{})
;;                                         brick))
;;                            (assoc b brick (conj (get b brick #{})
;;                                                 (get occupied pos)))]
;;                           [a b]))
;;                       [above below]
;;                       (get-positions (step-down brick)))))
;;           [{} {}]
;;           fallen))

(defn check-fall-me [support-bricks support-by-bricks wouldfall brick]
  (if (some #{brick} wouldfall)
    wouldfall
    (->> (get support-bricks brick)
         (reduce (fn [newfall supported-brick]
                   (if (set/subset? (get support-by-bricks supported-brick) newfall)
                     (check-fall-me support-bricks support-by-bricks newfall supported-brick)
                     newfall))
                 (conj wouldfall brick)))))

(defn check-fall  [above below wouldfall brick]
  (c/insp [wouldfall brick])
  (if (some #{brick} wouldfall)
    wouldfall
    (reduce (fn [newfall abovebrick]
              (if (set/subset? (get below abovebrick) newfall)
                (check-fall above below newfall abovebrick)
                newfall))
            (conj wouldfall brick)
            (get above brick))))

(defn check-if-would-fall [brick support-bricks support-by-bricks]
  (->> (support-bricks brick)
       (map support-by-bricks)
       (filter #(= #{brick} %))
       (not-empty)))

(defn part1 [inp]
  (let [bricks (sort-by #(nth % 2) (parse-input inp))
        [fallen occupied] (fallme bricks)
        [support-bricks supporte-by-bricks] (supports-and-supported-by fallen occupied)]
    (- (count fallen)
       (->> fallen
            (filter #(check-if-would-fall % support-bricks supporte-by-bricks))
            (count)))))

(defn part2 [inp])

(part1 exp1-input)
(part1 part1-input)

