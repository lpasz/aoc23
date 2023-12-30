(ns aoc23.day22
  "Sand Slabs"
  (:require [core :as c]
            [clojure.set :as set]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))
(def part1-other-input (c/get-input "part1-other.txt"))

(defn rangex [p1 p2]
  (range (min p1 p2) (inc (max p1 p2))))

(defn by-first-z [_id [[_x _y z]]] z)

(defn parse [inp]
  (->> (re-seq #"(?m)(\d+),(\d+),(\d+)\~(\d+),(\d+),(\d+)" inp)
       (map rest)
       (map #(map c/parse-int %))
       (map-indexed (fn [idx [x1 y1 z1 x2 y2 z2]]
                      [;; id's
                    ;;    (char (+ 65 idx))
                       idx
                       (for [x (rangex x1 x2)
                             y (rangex y1 y2)
                             z (rangex z1 z2)]
                         [x y z])]))
       (into (sorted-map))))

(def p1 (parse part1-input))

(def floor #{1})

(defn down-one [[x y z]]
  (let [new-z  (if (floor z)
                 z
                 (dec z))]
    [x y new-z]))

(defn brick-fall [brick occupied-space]
  (let [brick-after-gravity (map down-one brick)]
    (cond (= brick brick-after-gravity) nil
          (some occupied-space brick-after-gravity) nil
          :else (or (brick-fall brick-after-gravity occupied-space) brick-after-gravity))))

(defn closest-to-floor [[_ coords]]
  (->> coords
       (map last)
       (reduce + Integer/MAX_VALUE)))

(defn after-falling [bricks]
  (loop [bricks (c/queue (sort-by closest-to-floor bricks))
         occupied-space (->> bricks (vals) (c/flatten-once) (set))
         after-falling (sorted-map)]
    (if (empty? bricks)
      after-falling
      (let [[id brick] (peek bricks)
            bricks (pop bricks)
            occupied-space-rm (reduce disj occupied-space brick)
            brick-after-fall (brick-fall brick occupied-space-rm)]
        (if (nil? brick-after-fall)
          (recur bricks
                 occupied-space
                 (assoc after-falling id brick))
          (recur bricks
                 (reduce conj occupied-space-rm brick-after-fall)
                 (assoc after-falling id brick-after-fall)))))))

(defn after-falling-all [bricks]
  (c/insp ".")
  (let [after (after-falling bricks)]
    (if (= bricks after)
      after
      (recur after))))

(after-falling-all (parse part1-input))

(def bricks (after-falling-all (parse part1-input)))
(def bricks-other (after-falling (parse part1-other-input)))
(def ex-bricks (after-falling (parse exp1-input)))



(defn part1 [bricks]
  (->> (keys bricks)
       (map #(dissoc bricks %))
       ;; nothing moved if brick is removed
       (filter #(not= % (after-falling %)))
       (count)
       (- (count bricks))))

(part1 bricks)


(defn parse-input [inpup]
  (->> (str/split input  #"\n")
       (mapv (fn [line] (vec (flatten (map #(map parse-long
                                                 (str/split % #","))
                                           (str/split line #"~"))))))))





;; too high 900
;; too high 895
;; too high 444
;; unknown
;; 392 wrong
;; (part1 exp1-input)
;; (part1 part1-input)
