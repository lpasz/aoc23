(ns aoc23.day2
  (:require [clojure.string :as str]
            [core :refer [max-by]]))

(def exp1-input (slurp "./inputs/day2/exp1.txt"))
(def part1-input (slurp "./inputs/day2/part1.txt"))

(def can-possibly-contain-cubes {:red 12 :green 13 :blue 14})

(defn- to-cube-color-and-count [cube-count-and-color]
  (let [[count color] (-> cube-count-and-color
                          (str/trim)
                          (str/split #"\s"))]
    [(keyword color) (Integer/parseInt count)]))

(defn to-cubes-taken-out-of-bag [cubes-taken-out-of-bag]
  (->> (str/split cubes-taken-out-of-bag #",")
       (map to-cube-color-and-count)
       (into {})))

(defn- to-taken-out-of-bag-in-turns [turns]
  (->> (str/split turns  #";")
       (map to-cubes-taken-out-of-bag)))

(defn- to-id-and-bag-turns [line]
  (let [[[_line id turns]] (re-seq #"Game\s(\d+):\s(.*)" line)]
    [(Integer/parseInt id) (to-taken-out-of-bag-in-turns turns)]))

(defn- possibly-contained-in-round? [round]
  (every? (fn [[color count]]
            (<= (get round color 0) count))
          can-possibly-contain-cubes))

(defn- possibly-contained-in-rounds? [[_id rounds]]
  (every? possibly-contained-in-round? rounds))

(defn part1 [inp]
  (->> (str/split-lines inp)
       (map to-id-and-bag-turns)
       (filter possibly-contained-in-rounds?)
       (map first)
       (apply +)))


(defn- power-of-cubes [[_id turns]]
  (let [color-max-cubes (fn [color] (max-by #(get % color 0) turns))]
    (->> (keys can-possibly-contain-cubes)
         (map color-max-cubes)
         (apply *))))

(defn part2 [inp]
  (->> (str/split-lines inp)
       (map to-id-and-bag-turns)
       (map power-of-cubes)
       (apply +)))

(comment
  ;; example 1
  (assert (= 8 (part1 exp1-input)))
  ;; part 1
  (assert (= 2317 (part1 part1-input)))
  ;; example 2
  (assert (= 2286 (part2 exp1-input)))
  ;; part 2
  (assert (= 74804 (part2 part1-input))))




