(ns aoc23.day1
  "Trebuchet?!"
  (:require [clojure.string :as str]
            [core :refer [map-key]]
            [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp2-input (c/get-input "exp2.txt"))
(def part1-input (c/get-input "part1.txt"))

(def digits
  "Map the text of digit to it's number"
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn- create-digits-re [digits]
  (->> (keys digits)
       (cons "\\d")
       (str/join "|")
       (re-pattern)))

(def re-digits (create-digits-re digits))
(def reverse-re-digits
  (->> (map-key str/reverse digits)
       (into {})
       (create-digits-re)))

(defn- to-number-digits [mixed-digits]
  (map #(get digits % %) mixed-digits))

(defn- find-first [text]
  (re-find re-digits text))

(defn- find-last [text]
  (->> (str/reverse text)
       (re-find reverse-re-digits)
       (str/reverse)))

(defn part1 [inp]
  (->> (str/split inp #"\n")
       (map #(re-seq #"\d" %))
       (map #(str/join [(first %) (last %)]))
       (map c/parse-int)
       (apply +)))

(defn part2 [inp]
  (->> (str/split-lines inp)
       (map (juxt find-first find-last))
       (map to-number-digits)
       (map #(str/join "" %))
       (map c/parse-int)
       (apply +)))

(comment
  ;; example 1
  (assert (= 142 (part1 exp1-input)))
  ;; part 1
  (assert (= 54450 (part1 part1-input)))
  ;; example 2
  (assert (= 281 (part2 exp2-input)))
  ;; part 2
  (assert (= 54265 (part2 part1-input)))
  ;;
  )


