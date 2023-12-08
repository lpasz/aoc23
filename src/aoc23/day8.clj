(ns aoc23.day8
  "Haunted Wasteland"
  (:require [clojure.string :as str]
            [core :as c]
            [clojure.set :as set]))

(def exp1-input (slurp "./inputs/day8/exp1.txt"))
(def exp2-input (slurp "./inputs/day8/exp2.txt"))
(def exp3-input (slurp "./inputs/day8/exp3.txt"))
(def part1-input (slurp "./inputs/day8/part1.txt"))

(defn symbol-to-left-right [inp]
  (->> (str/split-lines inp)
       (map #(re-seq #"[A-Z|\d]+" %))
       (map (fn [[key left right]]
              [key {\L left \R right}]))
       (into (sorted-map))))

(defn parse-input [inp]
  (let [[left-right rest] (str/split inp #"\n\n")
        left-right (cycle (seq left-right))
        symbol-to-left-right-map (symbol-to-left-right rest)]
    [left-right symbol-to-left-right-map]))

(defn steps-to-ZZZ [[lr-cycle symbol-to-left-right-map]]
  (loop [current-point (ffirst symbol-to-left-right-map)
         steps 0]
    (if (= "ZZZ" current-point)
      steps
      (let [current-step (nth lr-cycle steps)
            next-point (get-in symbol-to-left-right-map [current-point current-turn])]
        (recur next-point (inc turns))))))

(defn turns-to-XXZ [[lr-cycle symbol-to-left-right-map] current-point]
  (loop [current-point current-point
         step 0
         result []]
    (if (>= (count result) 10)
      result
      (recur ((symbol-to-left-right-map current-point) (nth lr-cycle step))
             (inc step)
             (if (str/ends-with? current-point "Z")
               (conj result step)
               result)))))

(defn part1 [inp]
  (->> (parse-input inp)
       (steps-to-ZZZ)))




;; (assert (= 2 (part1 exp1-input)))
;; (assert (= 6 (part1 exp2-input)))

(let [[lr remap] (parse-input part1-input)
      starts (->> remap
                  (filter #(str/ends-with? (first %) "A"))
                  (map first))]
  (->> (for [start starts]
         (steps-to-XXZ [lr remap] start))))

'([17621 35242 52863 70484 88105 105726 123347 140968 158589 176210]
  [16043 32086 48129 64172 80215 96258 112301 128344 144387 160430]
  [20777 41554 62331 83108 103885 124662 145439 166216 186993 207770]
  [19199 38398 57597 76796 95995 115194 134393 153592 172791 191990]
  [18673 37346 56019 74692 93365 112038 130711 149384 168057 186730]
  [12361 24722 37083 49444 61805 74166 86527 98888 111249 123610])

(apply set/intersection (for [x ]
                          (set (for [y (range 1 100000000)]
                                 (* x y)))))


;;wrong 26028269013987661603578457
(* 17621N 16043N 20777N 19199N 18673N 12361N)

(quot 26028269013987661603578457 263)