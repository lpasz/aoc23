(ns aoc23.day22
  "Sand Slabs"
  (:require [core :as c]
            [clojure.set :as set]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))
(def part1-other-input (c/get-input "part1-other.txt"))

(defn by-z [[_x _y z]] z)

(defn parse [inp]
  (->> (re-seq #"(?m)(\d+),(\d+),(\d+)\~(\d+),(\d+),(\d+)" inp)
       (map rest)
       (map #(map c/parse-int %))
       (map-indexed (fn [idx [x1 y1 z1 x2 y2 z2]]
                      (cond (= [x1 y1 z1] [x2 y2 z2]) [idx [[x2 y2 z2]]]
                            (= [x1 y1]    [x2 y2])    [idx (sort-by by-z (map #(vector x1 y1 %) (range (min z1 z2) (inc (max z1 z2)))))]
                            (= [x1 z1]    [x2 z2])    [idx (sort-by by-z (map #(vector x1 % z1) (range (min y1 y2) (inc (max y1 y2)))))]
                            (= [y1 z1]    [y2 z2])    [idx (sort-by by-z (map #(vector % y1 z1) (range (min x1 x2) (inc (max x1 x2)))))])))
       (into (sorted-map))))

(defn by-first-z [[_ [[_ _ z]]]] z)

(def floor #{1})

(defn down-one [[x y z]]
  (let [new-z  (if (floor z)
                 z
                 (dec z))]
    [x y new-z]))

(defn move-down-if-possible [b bset]
  (let [down (map (fn [[x y z]] [x y (if (floor z) 1 (dec z))]) b)]
    (when (and (not= down b) (every? #(not (bset %)) down))
      (or (move-down-if-possible down bset) down))))

(defn gravity [brick-coords occupied-spaces]
  (let [brick-coords-down (map down-one brick-coords)]
    (when-not (or (= brick-coords brick-coords-down)
                  (some occupied-spaces brick-coords-down))
      (or (move-down-if-possible brick-coords-down occupied-spaces)
          brick-coords-down))))

(defn brick-drop [bricks]
  (loop [bseq (sort-by by-first-z bricks)
         bricks bricks
         occupied-space (set (c/flatten-once (vals bricks)))]
    (if (empty? bseq)
      bricks
      (let [[id brick-coords] (first bseq)
            bseq (rest bseq)
            bricksn (dissoc bricks id)
            occupied-space-after (reduce disj occupied-space brick-coords)
            brick-coords-down (gravity brick-coords occupied-space-after)]
        (if-not (nil? brick-coords-down)
          (recur bseq
                 (assoc bricksn id brick-coords-down)
                 (reduce conj occupied-space-after brick-coords-down))
          (recur bseq
                 bricks
                 occupied-space))))))

(def bricks (brick-drop (parse part1-input)))
(def bricks-other (brick-drop (parse part1-other-input)))
(def ex-bricks (brick-drop (parse exp1-input)))

(defn part1 [bricks]
  (->> (keys bricks)
       (map (fn [b-id] (print (str "brick id " b-id " "))
              b-id))
       (map #(dissoc bricks %))
       ;; nothing moved if brick is removed
       (filter (fn [brickless]
                 (c/insp (= brickless (brick-drop brickless)))))
       (count)))

(part1 ex-bricks)

;; too high 900
;; too high 895
;; too high 444
;; unknown
;; 392 wrong
;; (part1 exp1-input)
;; (part1 part1-input)
