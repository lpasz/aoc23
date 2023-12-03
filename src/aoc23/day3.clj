(ns aoc23.day3
  (:require [clojure.string :as str]
            [core :refer [insp flatten-once]]
            [clojure.set :refer [intersection]]))

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

(defn keep-surrounded [items]
  (let [positions-with-symbol (->> items
                                   (filter (fn [[k _]] (string? k)))
                                   (map (fn [[_ [v]]] v))
                                   (mapcat surroundings)
                                   (into #{}))
        _ (insp positions-with-symbol)
        number-and-pos (filter (fn [[k _]] (not (string? k))) items)]
    (->> number-and-pos
         (keep (fn [[item positions]]
                 (let [around (into #{} positions)]
                   (when (not-empty (intersection around positions-with-symbol))
                     item)))))))

(assert (= #{[-1 -1]  [0 -1] [1 -1]
             [-1  0]         [1  0]
             [-1  1]  [0  1] [1  1]} (into #{} (surroundings [0 0]))))


(defn part1 [inp]
  (->> (str/split-lines inp)
       (keep-indexed (fn [idy line]
                       (not-empty (second (reduce (fn [[idx acc] item]
                                                    (cond
                                                      (= item ".") [(inc idx) acc]
                                                      (int? (try-int item)) (let [x-positions (range (count item))
                                                                                  last-x (+ idx (apply max x-positions))
                                                                                  item (or (try-int item) item)
                                                                                  xy-positions (mapv (fn [i] [(+ idx i) idy]) x-positions)]
                                                                              [(inc last-x) (conj acc [item xy-positions])])
                                                      :else [(inc idx) (conj acc [item [[idx idy]]])]))
                                                  [0 []]
                                                  (re-seq #"\d+|\.|." line))))))
       (flatten-once)))



(apply + (keep-surrounded (part1 part1-input)))


(part1 "...............*......171*......................714.....543............737.....372.............941.............113..*....=.......853....733.")

;;wrong 515436