(ns aoc23.day25
  "Snowverload"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- parse [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #"\:|\s"))
       (map (fn [[key _ & vals]] [key (set vals)]))
       (into {})))