(ns aoc23.day3
  (:require [clojure.string :as str]
            [core :refer [insp flatten-once]]))

;; don't forget to add the text in there.
(def exp1-input (slurp "./inputs/day3/exp1.txt"))
(def part1-input (slurp "./inputs/day3/part1.txt"))

(defn surroundings [[x y]]
  [[(dec x) y] [(dec x) (inc y)] [(dec x) (dec y)]
   [x (inc y)]                   [x (dec y)]
   [(inc x) y] [(inc x) (dec y)] [(inc x) (inc y)]])

(defn try-int [int]
  (try (Integer/parseInt int)
       (catch Exception e nil)))

(try-int "312")

(defn keep-surrounded [items]
  (let [positions-with-symbol (->> items
                                   (insp :items)
                                   (filter (fn [[k _]] (string? k)))
                                   (insp :positions-with-symbol)
                                   (map (fn [[_ [v]]] v))
                                   (into #{}))
        number-and-pos (filter (fn [[k _]] (not (string? k))) items)]
    (->> number-and-pos
         (keep (fn [[item positions]]
                 (let [around (->> positions (mapcat surroundings) (into #{}))]
                   (when (some around positions-with-symbol)
                     item)))))))

(some #{[6 3] [5 8] [5 5] [3 8]}
      #{[2 5] [4 7] [4 6] [5 7] [1 5] [1 7] [5 6] [5 5] [2 7] [3 6] [4 5] [1 6] [3 7] [2 6] [3 5]})

(assert (= #{[-1 -1] [0 -1] [1 -1]
             [-1 0]         [1 0]
             [-1 1]  [0 1]  [1 1]} (into #{} (surroundings [0 0]))))


(defn part1 [inp]
  (->> (str/split-lines inp)
       (keep-indexed (fn [idy line]
                       (not-empty (second (reduce (fn [[idx acc] item]
                                                    (cond
                                                      (= item ".") [(inc idx) acc]
                                                      (int? (try-int item)) (let [x-positions (range (count item))
                                                                                  last-x (apply max x-positions)
                                                                                  item (or (try-int item) item)
                                                                                  xy-positions (map (fn [i] [(+ idx i) idy]) x-positions)]
                                                                              [(inc last-x) (conj acc [item xy-positions])])
                                                      :else [(inc idx) (conj acc [item [[idx idy]]])]))
                                                  [0 []]
                                                  (re-seq #"\d+|\.|." line))))))
       (flatten-once)))

(part1 exp1-input)

(filter (fn [[k v]] (string? k)) (part1 exp1-input))

(apply + (keep-surrounded (part1 part1-input)))

(->> (part1 exp1-input)
     (filter (fn [[key value]] [key])))

(surroundings [4 0])