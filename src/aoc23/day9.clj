(ns aoc23.day9
  "Mirage Maintenance"
  (:require [clojure.string :as str]
            [core :as c]))


(def exp1-input (slurp "./inputs/day9/exp1.txt"))
(def part1-input (slurp "./inputs/day9/part1.txt"))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-input [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #" "))
       (mapv #(mapv parse-int %))))

(defn diff [coll]
  (->> (partition 2 1 coll)
       (map (fn [[x y]] (- y x)))))

(defn diff-to-zero [coll]
  (loop [colls [coll]
         coll coll]
    (let [diff (diff coll)]
      (if (every? zero? diff)
        (conj colls diff)
        (recur (conj colls diff) diff)))))

(defn part1 [inp]
  (->> (parse-input inp)
       (map diff-to-zero)
       (map (fn [coll] (map #(or (last %) 0) coll)))
       (map #(apply + %))
       (apply +)))

(defn part2 [inp]
  (->> (parse-input inp)
       (map reverse)
       (map diff-to-zero)
       (map (fn [coll] (map #(or (last %) 0) coll)))
       (map #(apply + %))
       (apply +)))




(part1 exp1-input)
(part1 part1-input)
(part2 part1-input)
