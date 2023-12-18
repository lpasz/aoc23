(ns aoc23.day3
  "Gear Ratios"
  (:require [clojure.string :as str]
            [core :as c]
            [clojure.set :as set]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- symbols-xy [items]
  (->> items
       (c/filter-by-key string?)
       (map (comp first second))
       (mapcat c/surrounding-xy)))

(defn- number-surrounded-by-symbol [xy-positions symbol-xy-positions]
  (-> (set xy-positions)
      (set/intersection (set symbol-xy-positions))
      (not-empty)))

(defn- keep-part-numbers [items]
  (let [symbols-xy (symbols-xy items)]
    (->> items
         (c/filter-by-key int?)
         (c/filter-by-value #(number-surrounded-by-symbol %1 symbols-xy))
         (map first))))

(defn- number-xy-positions [start-idx idy item acc]
  (let [x-xy-positions (map #(+ start-idx %) (range (count item)))
        next-x (inc (last x-xy-positions))
        int-item (c/parse-int item)
        xy-positions (mapv (fn [x] [x idy]) x-xy-positions)
        acc (conj acc [int-item xy-positions])]
    [next-x acc]))


(defn- item-and-xy-positions [[idx acc] item idy]
  (cond
    ;; we ignore "." and increment the idx
    (= item ".") [(inc idx) acc]
    ;; if it's a number, we parse it, and increment idx depending on the size of number
    (int? (c/parse-int item)) (number-xy-positions idx idy item acc)
    ;; if symbol is one caracter we add it's only positions
    :else [(inc idx) (conj acc [item [[idx idy]]])]))

(defn- parse-line-to-items-and-xy-positions [idy line]
  (->> line
       (re-seq #"\d+|\.|.")
       (reduce #(item-and-xy-positions %1 %2 idy) [0 []])
       (second)
       (not-empty)))

(defn- parse-input [inp]
  (->> (str/split-lines inp)
       (keep-indexed parse-line-to-items-and-xy-positions)
       (c/flatten-once)))

(defn- gear-xy-positions [items]
  (->> items
       (c/filter-by-key #(= "*" %))
       (map (comp first second))
       (into #{})))

(defn- number-surroundings [items]
  (->> items
       (c/filter-by-key int?)
       (map (c/fnvec identity #(into #{} (mapcat c/surrounding-xy %))))))

(defn- number-surrounded-by-gear [[number surrounding-xy-positions] gear-position]
  (when (surrounding-xy-positions gear-position)
    number))

(defn- multiply-2-numbers-connected-by-gear [gear-position numbers-surroundings]
  (let [connected-by-a-gear (keep #(number-surrounded-by-gear %1 gear-position) numbers-surroundings)]
    (when (= 2 (count connected-by-a-gear))
      (apply * connected-by-a-gear))))

(defn- keep-gears-that-multiply-2-numbers [items]
  (keep #(multiply-2-numbers-connected-by-gear % (number-surroundings items)) (gear-xy-positions items)))

(defn part1 [inp]
  (->> (parse-input inp)
       (keep-part-numbers)
       (apply +)))

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
  