(ns aoc23.day22
  "Sand Slabs"
  (:require [core :as c]
            [clojure.set :as set]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

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
       (into {})))

(defn by-first-z [[_ [[_ _ z]]]] z)

(defn move-down-if-possible [b bset]
  (let [down (map (fn [[x y z]] [x y (if (= z 1) 1 (dec z))]) b)]
    (when (and (not= down b) (every? #(not (bset %)) down))
      (or (move-down-if-possible down bset) down))))

(defn brick-drop [bricks]
  (loop [bseq (sort-by by-first-z bricks)
         bricks bricks]
    (if (empty? bseq)
      bricks
      (let [[k b] (first bseq)
            bseq (rest bseq)
            bricksn (dissoc bricks k)
            occupied-space (->> bricksn
                                (vals)
                                (c/flatten-once)
                                (set))
            down (move-down-if-possible b occupied-space)]
        (if down
          (recur bseq (assoc bricksn k down))
          (recur bseq bricks))))))

(def supports inc)
(def supported dec)

(defn check-if-brick [bricks bfn]
  (let [rev-bricks (c/revert-map bricks)]
    (loop [bseq (seq bricks)
           sups-by {}]
      (if (empty? bseq)
        sups-by
        (let [[k b] (first bseq)]
          (if-let [sup-by (->> (keep (fn [[x y z]] (let [k1 (first (rev-bricks [x y (bfn z)]))]
                                                     (when-not (= k1 k)
                                                       k1))) b)
                               (set))]
            (recur (rest bseq) (reduce (fn [acc s] (update acc k #(conj % s))) sups-by sup-by))
            (recur (rest bseq) sups-by)))))))

(defonce bricks (brick-drop (parse part1-input)))
(defonce ex-bricks (brick-drop (parse exp1-input)))

(defn supported-by-more-than-one [bricks-supported-by]
  (reduce (fn [acc [_ vs]]
            (if (> (count vs) 1)
              (into acc vs)
              acc))
          #{}
          bricks-supported-by))

(defn supported-by-one [bricks-supported-by]
  (reduce (fn [acc [_ vs]]
            (if (= (count vs) 1)
              (into acc vs)
              acc))
          #{}
          bricks-supported-by))

(defn part1-f [b]
  (let [supported-by (check-if-brick b supported)
        supports (check-if-brick b supports)
        not-support (set/difference (set (keys b)) (set (keys supports)))]
    
    (set/difference (into not-support (supported-by-more-than-one supported-by))
     (supported-by-one supported-by)
    )))



(keys ex-bricks)
(keys {0 '(2 1), 1 '(3 4), 2 '(3 4), 3 '(5), 4 '(5), 5 '(6)})
(count (part1-f bricks))

(part1-f ex-bricks)
(count (part1-f bricks))

(set/difference #{0 1 2 3 4 5 6} #{0 1 2 3 4 5})

(+ 333 51)



;; too high 900
;; too high 895
;; too high 444
;; unknown
;; 392 wrong
;; (part1 exp1-input)
;; (part1 part1-input)
