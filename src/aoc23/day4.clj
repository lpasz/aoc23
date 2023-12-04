(ns aoc23.day4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def exp1-input (slurp "./inputs/day4/exp1.txt"))
(def part1-input (slurp "./inputs/day4/part1.txt"))

(defn- parse-digits [txt]
  (->> (re-seq #"\d+" txt)
       (map #(Integer/parseInt %))))

(defn- calc-points [my-wins]
  (if-not (= (count my-wins) 0)
    (->> (repeat (count my-wins) 1)
         (reduce #(* 2 %1 %2) 1/2))
    0))

(defn- parse-input [inp]
  (->> (str/split-lines inp)
       (map #(re-seq #"(.*):(.*)\|(.*)" %))
       (map (fn [[[_line card-num win-nums my-nums]]]
              (let [card-id (first (parse-digits card-num))
                    win-nums (set (parse-digits win-nums))
                    my-nums (set (parse-digits my-nums))
                    my-wins (set/intersection win-nums my-nums)]
                [card-id {:scorecard/number-of-copies 1
                          :scorecard/points (calc-points my-wins)
                          :scorecard/winning-numbers-count (count my-wins)}])))
       (into (sorted-map))))

(defn part1 [inp]
  (->> (parse-input inp)
       (vals)
       (map :scorecard/points)
       (apply +)))

(defn- inc-number-of-copies-of-next-n-scorecard [scorecards scorecard-to-inc amount]
  (reduce (fn [scorecards scorecard-to-inc]
            (update-in scorecards [scorecard-to-inc :scorecard/number-of-copies] #(+ amount %)))
          scorecards
          scorecard-to-inc))

(defn- recalc-number-of-copies [scorecards]
  (reduce (fn [scorecards id]
            (let [win-count (get-in scorecards [id :scorecard/winning-numbers-count])
                  number-of-copies (get-in scorecards [id :scorecard/number-of-copies])
                  scorecard-to-inc (range (inc id) (inc (+ id win-count)))]
              (inc-number-of-copies-of-next-n-scorecard scorecards
                                                        scorecard-to-inc
                                                        number-of-copies)))
          scorecards
          (keys scorecards)))

(defn part2 [inp]
  (->> (parse-input inp)
       (recalc-number-of-copies)
       (vals)
       (map :scorecard/number-of-copies)
       (apply +)))

(assert (= 13 (part1 exp1-input)))
(assert (= 17803 (part1 part1-input)))
(assert (= 30 (part2 exp1-input)))
(assert (= 5554894 (part2 part1-input)))