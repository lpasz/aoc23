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

(defn count-patterns [[text recovery]]
  (let [sum (reduce + recovery)
        regx-pattern (pattern recovery)
        ?count (count (filter #(= % \?) text))
        hash-count (count (filter #(= \# %) text))
        need-n-hash (- sum hash-count)
        options (map (fn [_] ["#" "."]) (range 20))]
    (->>
     (for [a0  ["#" "."] a1  ["#" "."] a2  ["#" "."] a3  ["#" "."]
           a4  ["#" "."] a5  ["#" "."] a6  ["#" "."] a7  ["#" "."]
           a8  ["#" "."] a9  ["#" "."] a10 ["#" "."] a11 ["#" "."]
           a12 ["#" "."] a13 ["#" "."] a14 ["#" "."] a15 ["#" "."]
           a16 ["#" "."] a17 ["#" "."] a18 ["#" "."] a19 ["#" "."]
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

;; ???.### 1,1,3

;;  "#.#.###"
;; (bit-and 2r1010111 2r1)


(defn part1 [inp]
  (->> (parse-input inp)
       (map count-patterns)
       (reduce +)))

;; (time (part1 exp1-input))
;; (part1 part1-input)

(mapcat identity (repeat 3 [1 2 3]))

(defn part2fy [[a b]]
  [(str/join "?" (repeat 5 a))
   (mapcat identity (repeat 5 b))])

(->> (parse-input exp1-input)
     (map part2fy))

(doseq [[a b] (map part2fy (parse-input exp1-input))]
  (c/insp [a b (count-patterns [a b])]))


(assert (= ["???.###????.###????.###????.###????.###" [1,1,3,1,1,3,1,1,3,1,1,3,1,1,3]]
           (part2fy ["???.###" [1,1,3]])))