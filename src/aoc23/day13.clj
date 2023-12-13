(ns aoc23.day13
  "Point of Incidence"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn reflection? [idx-mtx idx1 line1 idx2 line2]
  (cond
    (or (nil? line1) (nil? line2)) true
    (not= line1 line2) false
    (= line1 line2) (recur idx-mtx
                           (dec idx1) (get idx-mtx (dec idx1))
                           (inc idx2) (get idx-mtx (inc idx2)))))


(defn reflections-lines [mtx]
  (let [idx-mtx (into (sorted-map) (map-indexed (fn [idx line] [(inc idx) line]) mtx))]
    (->> (partition 2 1 idx-mtx)
         (filter (fn [[[idx1 line1] [idx2 line2]]] (reflection? idx-mtx idx1 line1 idx2 line2)))
         (first)
         (c/then [[[idx1 _line1] [idx2 _line2]]] [idx1 idx2]))))

(defn reflections-columns [mtx]
  (->> (c/transpose mtx)
       (reflections-lines)))

(defn find-reflections [mtx]
  {:lines (reflections-lines mtx)
   :columns (reflections-columns mtx)})

(defn to-mtx-with-coords [lines]
  (map (fn [line] (seq line)) (str/split-lines lines)))

(defn calc-reflections [reflections]
  (cond
    (= [nil nil] (:lines reflections))
    (first (:columns reflections))

    (= [nil nil] (:columns reflections))
    (* 100 (first (:lines reflections)))))

(defn reflection? [idx-mtx idx1 line1 idx2 line2]
  (cond
    (or (nil? line1) (nil? line2)) true
    (not= line1 line2) false
    (= line1 line2) (recur idx-mtx
                           (dec idx1) (get idx-mtx (dec idx1))
                           (inc idx2) (get idx-mtx (inc idx2)))))

(defn part1 [inp]
  (->> (str/split inp #"\n\n")
       (map to-mtx-with-coords)
       (map find-reflections)
       (map calc-reflections)
       (reduce +)))

(defn part2 [inp]
  (->> (str/split inp #"\n\n")
       (map to-mtx-with-coords)
       (map find-reflections)
       (map calc-reflections)
       (reduce +)))

(assert (= 405 (part1 exp1-input)))
(assert (= 29846 (part1 part1-input)))
