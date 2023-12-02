(ns aoc23.day2
  (:require [clojure.string :as str]))

(def exp1-input (slurp "./inputs/day2/exp1.txt"))
(def part1-input (slurp "./inputs/day2/part1.txt"))

(def can-contain {:red 12 :green 13 :blue 14})

(defn to-turn-item [turn-item]
  (let [[count color] (str/split (str/trim turn-item) #"\s")]
    [(keyword color) (Integer/parseInt count)]))

(defn to-turn [turn]
  (->> (str/split turn #",")
       (map to-turn-item)
       (into {})))

(defn to-contents [contents]
  (->> (str/split contents  #";")
       (map to-turn)
       (into [])))

(defn by-id [line]
  (let [[[_all _game id contents]] (re-seq #"(Game\s)(\d+):\s(.*)" line)]
    [(Integer/parseInt id)
     (to-contents contents)]))

(defn can-contain-round? [round]
  (every? (fn [[color quantity]]
            (<= (get round color 0) quantity))
          can-contain))

(defn can-contain-rounds? [[_id rounds]]
  (every? can-contain-round? rounds))

(defn part1 [text]
  (->> (str/split-lines text)
       (map by-id)
       (filter can-contain-rounds?)
       (map first)
       (apply +)))

(defn power-of-cubes [[_id turns]]
  (* (apply max (->> turns (map #(get % :red 0))))
     (apply max (->> turns (map #(get % :green 0))))
     (apply max (->> turns (map #(get % :blue 0))))))

(defn part2 [text]
  (->> (str/split-lines text)
       (map by-id)
       (map power-of-cubes)
       (apply +)))

(comment
  ;; example 1
  (assert (= 8 (part1 exp1-input)))
  ;; part 1
  (assert (= 0 (part1 part1-input)))
  ;; example 2
  (assert (= 2286 (part2 exp1-input)))
  ;; part 2
  (assert (= 74804 (part2 part1-input))))




