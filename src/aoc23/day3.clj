(ns aoc23.day3
  (:require [clojure.string :as str]
            [core :refer [parse-int filter-by-key flatten-once surrounding-xy]]
            [clojure.set :refer [intersection]]))

(def exp1-input (slurp "./inputs/day3/exp1.txt"))
(def part1-input (slurp "./inputs/day3/part1.txt"))

(defn- symbols-xy [items]
  (->> items
       (filter (fn [[k _]] (string? k)))
       (map (fn [[_ [v]]] v))
       (mapcat surrounding-xy)))

(defn- number-surrounded-by-symbol [xy-xy-positions symbol-xy-xy-positions]
  (not-empty (intersection (set xy-xy-positions) (set symbol-xy-xy-positions))))

(defn- keep-part-numbers [items]
  (->> items
       (filter-by-key int?)
       (keep (fn [[item xy-xy-positions]]
               (when (number-surrounded-by-symbol xy-xy-positions (symbols-xy items))
                 item)))))

(defn- number-xy-positions [start-idx idy item acc]
  (let [x-xy-positions (map #(+ start-idx %) (range (count item)))
        next-x (inc (last x-xy-positions))
        int-item (Integer/parseInt item)
        xy-xy-positions (mapv (fn [x] [x idy]) x-xy-positions)
        acc (conj acc [int-item xy-xy-positions])]
    [next-x acc]))


(defn- item-and-xy-positions [[idx acc] item idy]
  (cond
    ;; we ignore .
    (= item ".") [(inc idx) acc]
    ;; if item is bigger than 1 position it's a number
    (int? (parse-int item)) (number-xy-positions idx idy item acc)
    ;; if symbol is one caracter we add it's only positions
    :else [(inc idx) (conj acc [item [[idx idy]]])]))

(defn parse-line-to-items-and-xy-xy-positions [idy line]
  (->> line
       (re-seq #"\d+|\.|.")
       (reduce #(item-and-xy-positions %1 %2 idy) [0 []])
       (second)
       (not-empty)))

(defn- parse-input [inp]
  (->> (str/split-lines inp)
       (keep-indexed parse-line-to-items-and-xy-xy-positions)
       (flatten-once)))

(defn part1 [inp]
  (->> (parse-input inp)
       (keep-part-numbers)
       (apply +)))

(defn- gear-xy-positions [items]
  (->> items
       (filter (fn [[k _]] (= "*" k)))
       (map (fn [[_ [v]]] v))
       (into #{})))

(defn- number-surroundings [items]
  (->> items
       (filter (fn [[k _]] (int? k)))
       (map (fn [[k v]] [k (into #{} (mapcat surrounding-xy v))]))))

(defn- number-surrounded-by-gear [[number surrounding-xy-positions] gear-position]
  (when (surrounding-xy-positions gear-position)
    number))

(defn- multiply-2-numbers-connected-by-gear [gear-position numbers-surroundings]
  (let [connected-by-a-gear (keep #(number-surrounded-by-gear %1 gear-position) numbers-surroundings)]
    (when (= 2 (count connected-by-a-gear))
      (apply * connected-by-a-gear))))

(defn- keep-gears-that-multiply-2-numbers [items]
  (keep #(multiply-2-numbers-connected-by-gear % (number-surroundings items)) (gear-xy-positions items)))

(defn part2 [inp]
  (->> (parse-input inp)
       (keep-gears-that-multiply-2-numbers)
       (apply +)))

(comment
  ;; exp 1
  (assert (= 4361 (part1 exp1-input)))
  ;; part 1
  (assert (= 533775 (part1 part1-input)))
  ;; exp 2
  (assert (= 467835 (part2 exp1-input)))
  ;; part 2
  (assert (= 78236071 (part2 part1-input)))
  ;;
  )
  