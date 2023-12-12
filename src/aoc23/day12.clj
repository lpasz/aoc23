(ns aoc23.day12
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn parse-input [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #" "))
       (map (fn [[spring-data rec-data]]
              [spring-data (->> rec-data
                                (re-seq #"\d+")
                                (map c/parse-int))]))))

(defn pattern [recovery]
  (->> recovery
       (map (fn [i] (str "#{" i "}")))
       (str/join "\\.+")
       (c/then [s] (str "[\\.+]?" s "[\\.+]?"))
       (re-pattern)))

(defn count-patterns [text recovery]
  (let [sum (reduce + recovery)
        regx-pattern (pattern recovery)
        ?count (count (filter #(= % \?) text))
        hash-count (count (filter #(= \# %) text))
        need-n-hash (- sum hash-count)
        options (map (fn [_] ["#" "."]) (range 20))]
    (->>
     (for [a0 (nth options 0)
           a1 (nth options 1)
           a2 (nth options 2)
           a3 (nth options 3)
           a4 (nth options 4)
           a5 (nth options 5)
           a6 (nth options 6)
           a7 (nth options 7)
           a8 (nth options 8)
           a9 (nth options 9)
           a10 (nth options 10)
           a11 (nth options 11)
           a12 (nth options 12)
           a13 (nth options 13)
           a14 (nth options 14)
           a15 (nth options 15)
           a16 (nth options 16)
           a17 (nth options 17)
           a18 (nth options 18)
           a19 (nth options 19)
           :when (= need-n-hash (->> [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]
                                     (take ?count)
                                     (filter #(= "#" %))
                                     (count)))]
       (->> [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19]
            (take ?count)
            (reduce #(str/replace-first %1 #"\?" %2) text)))
     (filter #(re-find regx-pattern %))
     (set)
     (count))))

(generate-strings "#??#???.??#?#?#??#?." [6,8,2])


