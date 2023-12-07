(ns aoc23.day7
  "Camel Cards"
  (:require [clojure.string :as str]
            [core :as c]))

(def exp1-input (slurp "./inputs/day7/exp1.txt"))
(def part1-input (slurp "./inputs/day7/part1.txt"))

(def card-to-int
  {\2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn parse-input [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #" "))
       (map (fn [[hand bid]]
              (let [hand (seq hand)
                    hand-int (map card-to-int hand)
                    hand-freq (frequencies hand-int)]
                {:bid (Integer/parseInt bid)
                 :hand hand
                 :hand-int hand-int
                 :hand-freq hand-freq})))))

(def hand-type-to-points {:five-of-a-kind 7
                  :four-of-a-kind 6
                  :full-house 5
                  :three-of-a-kind 4
                  :two-pair 3
                  :one-pair 2
                  :high-card 1})

(defn hand-type [{:keys [hand-freq]}]
  (case (sort > (vals hand-freq))
    [1 1 1 1 1] :high-card
    [2 1 1 1] :one-pair
    [2 2 1] :two-pair
    [3 1 1] :three-of-a-kind
    [3 2] :full-house
    [4 1] :four-of-a-kind
    [5] :five-of-a-kind))

(defn hand-type-points [hand]
  (-> hand
      (hand-type)
      (hand-type-to-points)))

(defn strongest-first-card [cards1 cards2]
  (loop [cards1  (:hand-int cards1)
         cards2 (:hand-int cards2)]
    (let [c1 (first cards1)
          c2 (first cards2)
          cmp (compare c1 c2)]
      (if (= cmp 0)
        (recur (rest cards1) (rest cards2))
        (compare c1 c2)))))

(defn type-of-hand-and-strongest-card [cards1 cards2]
  (let [hand-type1-points  (hand-type-points cards1)
        hand-type2-points (hand-type-points cards2)]
    (if (= hand-type1-points hand-type2-points)
      (strongest-first-card cards1 cards2)
      (compare hand-type1-points hand-type2-points))))

(defn part1 [inp]
  (->> (parse-input inp)
       (sort type-of-hand-and-strongest-card)
       (map-indexed (fn [idx cards] (assoc cards :rank (inc idx))))
       (map #(* (:bid %) (:rank %)))
       (apply +)))

;; (765 220 28 684 483) 
(assert (= 6440 (part1 exp1-input)))
(assert (= 0 (part1 part1-input)))

(->> (parse-input part1-input)
     (map :hand)
     (c/then [x] [(count x) (count (set x))]))