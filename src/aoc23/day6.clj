(ns aoc23.day6
  (:require [clojure.string :as str]
            [core :as c]))

(def exp1-input (slurp "./inputs/day6/exp1.txt"))
(def part1-input (slurp "./inputs/day6/part1.txt"))


(defn- hold-better-than-record [time record-distance hold]
  (let [distance (* hold (- time hold))]
    (when (> distance record-distance)
      {:hold hold
       :distance distance
       :time time
       :record-distance record-distance})))

(defn- holds-better-than-record [{:keys [time record-distance]}]
  (->> (range 0 (inc time))
       (keep #(hold-better-than-record time record-distance %))))

(defn part1 [inp]
  (->> (str/split-lines inp)
       (map #(re-seq #"\d+" %))
       (map (fn [line-nums] (map #(Integer/parseInt %) line-nums)))
       ((fn [[times distances]]
          (map (fn [time distance]
                 {:time time :record-distance distance})
               times
               distances)))
       (reduce #(* %1 (count (holds-better-than-record %2))) 1)))

(defn- hold-worst-than-record [time record-distance hold]
  (not (hold-better-than-record time record-distance hold)))

(defn- drop-while-worst-than-record [time record holds]
  (->> holds
       (drop-while #(hold-worst-than-record time record %))
       (first)))

(defn- holds-better-than-record-bigint [{:keys [time record-distance]}]
  (let [step 500
        drop-while-hold-worst-than-record #(drop-while-worst-than-record time record-distance %)
        near-first (->> (range 0 (inc time) step) drop-while-hold-worst-than-record)
        near-last (->> (range (inc time) 0 (- step)) drop-while-hold-worst-than-record)
        actual-first (->> (range (max 0 (- near-first step)) time) drop-while-hold-worst-than-record)
        actual-last (->> (range (min time (+ near-last step)) 0 -1) drop-while-hold-worst-than-record)]
    (inc (- actual-last actual-first))))

(defn part2 [inp]
  (->> (str/split-lines inp)
       (map #(str/join "" (re-seq #"\d" %)))
       (map biginteger)
       ((fn [[time distance]]
          {:time time
           :record-distance distance}))
       (holds-better-than-record-bigint)))

(assert (= 288 (part1 exp1-input)))
(assert (= 1195150 (part1 part1-input)))
(assert (= 71503 (part2 exp1-input)))
(assert (= 42550411 (part2 part1-input)))
