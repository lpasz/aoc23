(ns aoc23.day9
  "Mirage Maintenance"
  (:require [clojure.string :as str]
            [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- parse-input [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #" "))
       (map #(map c/parse-int %))))

(defn- diff-in-between-steps [coll]
  (->> (partition 2 1 coll)       
       (map (fn [[x y]] (- y x)))))

(defn- predict-next [coll]
  (loop [coll coll
         next-value (last coll)]
    (let [diff (diff-in-between-steps coll)]
      (if (every? zero? diff)
        next-value
        (recur diff (+ next-value (last diff)))))))

(defn part1 [inp]
  (->> (parse-input inp)
       (map predict-next)
       (reduce +)))

(defn part2 [inp]
  (->> (parse-input inp)
       (map reverse)
       (map predict-next)
       (reduce +)))

(comment
  ;; example 1 - part 1
  (assert (= 114 (part1 exp1-input)))
  ;; part1
  (assert (= 1974232246 (part1 part1-input)))
  ;; part 2
  (assert (= 928 (part2 part1-input)))
  ;;
  )