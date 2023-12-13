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

(defn one-diff? [line1 line2]
  (->> (map #(= %1 %2) line1 line2)
       (filter false?)
       (count)
       (c/one?)))

(defn reflection-diff?
  ([idx-mtx idx1 line1 idx2 line2] (reflection-diff? idx-mtx idx1 line1 idx2 line2 0))
  ([idx-mtx idx1 line1 idx2 line2 acc-diff]
   (cond (or (nil? line1)
             (nil? line2))              (c/one? acc-diff)

         (= line1 line2)                (recur idx-mtx
                                               (dec idx1) (get idx-mtx (dec idx1))
                                               (inc idx2) (get idx-mtx (inc idx2))
                                               acc-diff)

         (and (zero? acc-diff)
              (one-diff? line1 line2))  (recur idx-mtx
                                               (dec idx1) (get idx-mtx (dec idx1))
                                               (inc idx2) (get idx-mtx (inc idx2))
                                               1))))


(defn reflections-lines [mtx reflect?]
  (let [idx-mtx (into (sorted-map) (map-indexed (fn [idx line] [(inc idx) line]) mtx))]
    (->> (partition 2 1 idx-mtx)
         (filter (fn [[[idx1 line1] [idx2 line2]]] (reflect? idx-mtx idx1 line1 idx2 line2)))
         (first)
         (c/then [[[idx1 _line1] _]] idx1))))

(defn reflections-columns [mtx reflection?]
  (reflections-lines (c/transpose mtx) reflection?))

(defn find-reflections [mtx reflection?]
  {:lines (reflections-lines mtx reflection?)
   :columns (reflections-columns mtx reflection?)})

(defn to-mtx-with-coords [lines]
  (map seq (str/split-lines lines)))

(defn calc-reflections [{:keys [lines columns]}]
  (cond (nil? lines) columns
        (nil? columns) (* 100 lines)))

(defn part1 [inp]
  (->> (str/split inp #"\n\n")
       (map to-mtx-with-coords)
       (map #(find-reflections % reflection?))
       (map calc-reflections)
       (reduce +)))

(defn part2 [inp]
  (->> (str/split inp #"\n\n")
       (map to-mtx-with-coords)
       (map #(find-reflections % reflection-diff?))
       (map calc-reflections)
       (reduce +)))

(comment
  (assert (= 405 (part1 exp1-input)))
  (assert (= 29846 (part1 part1-input)))
  (assert (= 400 (part2 exp1-input)))
  (assert (= 25401 (part2 part1-input)))
  ;;
  )
