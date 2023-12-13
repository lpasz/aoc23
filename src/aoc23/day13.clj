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


(defn reflections-lines [mtx reflect?]
  (let [idx-mtx (into (sorted-map) (map-indexed (fn [idx line] [(inc idx) line]) mtx))]
    (->> (partition 2 1 idx-mtx)
         (filter (fn [[[idx1 line1] [idx2 line2]]] (reflect? idx-mtx idx1 line1 idx2 line2)))
         (first)
         (c/then [[[idx1 _line1] [_idx2 _line2]]] idx1))))

(defn reflections-columns [mtx reflection?]
  (reflections-lines (c/transpose mtx) reflection?))

(defn find-reflections [mtx reflection?]
  {:lines (reflections-lines mtx reflection?)
   :columns (reflections-columns mtx reflection?)})

(defn to-mtx-with-coords [lines]
  (map (fn [line] (seq line)) (str/split-lines lines)))

(defn calc-reflections [{:keys [lines columns]}]
  (cond (nil? lines) columns
        (nil? columns) (* 100 lines)))

(defn diff [line1 line2]
  (->> (map #(= %1 %2) line1 line2)
       (filter false?)
       (count)))

(defn reflection-diff?
  ([idx-mtx idx1 line1 idx2 line2] (reflection-diff? idx-mtx idx1 line1 idx2 line2 0))
  ([idx-mtx idx1 line1 idx2 line2 acc-diff]
;;    (c/insp [line1 line2 acc-diff (diff line1 line2) (= line1 line2)])
   (cond
     (and (= acc-diff 1) (or (nil? line1) (nil? line2))) true
     (or (nil? line1) (nil? line2)) false
     (= line1 line2) (recur idx-mtx
                            (dec idx1) (get idx-mtx (dec idx1))
                            (inc idx2) (get idx-mtx (inc idx2))
                            acc-diff)
     (and (zero? acc-diff) (= 1 (diff line1 line2))) (recur idx-mtx
                                                            (dec idx1) (get idx-mtx (dec idx1))
                                                            (inc idx2) (get idx-mtx (inc idx2))
                                                            1)
     :else false)))

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

(assert (= 405 (part1 exp1-input)))
(assert (= 29846 (part1 part1-input)))

(part2 part1-input)


(defn calc-reflections1 [{:keys [lines columns]}]
  (cond
    (= [nil nil] (:lines reflections))
    (or (first (:columns reflections)) 0)

    (= [nil nil] (:columns reflections))
    (* 100 (or (first (:lines reflections)) 0))))

(->> (map (fn [r1 r2] [r1 r2])

      ;; (fn [r1 r2]
      ;;       [r1 ]
      ;;       (cond
      ;;         (= (:lines r1) (:lines r2))
      ;;         {:lines [nil nil], :columns (:columns r2)}

      ;;         (= (:columns r1) (:columns r2))
      ;;         {:lines (:lines r2), :columns [nil nil]}

      ;;         :else r2))
          (->> (str/split part1-input #"\n\n")
               (map to-mtx-with-coords)
               (map #(find-reflections % reflection?)))
          (mp)(part2 part1-input)))

(filter #(=  {:lines [nil nil], :columns [nil nil]} %) (part2 part1-input))


;; too low 18390
;; too low 21590 
;; too high 37894