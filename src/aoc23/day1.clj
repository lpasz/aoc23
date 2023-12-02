(ns aoc23.day1
  (:require [clojure.string :as str]))

(def exp1-input (slurp "./inputs/day1/exp1.txt"))
(def exp2-input (slurp "./inputs/day1/exp2.txt"))
(def part1-input (slurp "./inputs/day1/part1.txt"))

(def digits #"\d")

(defn part1 [inp]
  (->> (str/split inp #"\n")
       (map #(re-seq digits %))
       (map #(str/join [(first %) (last %)]))
       (map #(Integer/parseInt %))
       (apply +)))

(def digits-remap {"one" "1"
                   "two" "2"
                   "three" "3"
                   "four" "4"
                   "five" "5"
                   "six" "6"
                   "seven" "7"
                   "eight" "8"
                   "nine" "9"})

(def reverse-digits-remap (->> digits-remap
                               (map (fn [[k v]] [(str/reverse k) v]))
                               (into {})))

(defn create-re [digits]
  (->> digits
       (keys)
       (cons "\\d")
       (str/join "|")
       (re-pattern)))

(def re-digits (create-re digits-remap))
(def reverse-re-digits (create-re reverse-digits-remap))

(defn- to-number-digits [mixed-digits]
  (map #(get digits % %) mixed-digits))

(defn- find-first [text]
  (re-find re-digits text))

(defn- find-last [text]
  (->> text
       (str/reverse)
       (re-find reverse-re-digits)
       (str/reverse)))

(defn part2 [inp]
  (->> (str/split inp #"\n")
       (map (fn [line] [(find-first line) (find-last line)]))
       (map to-number-digits)
       (map #(str/join "" %))
       (map #(Integer/parseInt %))
       (apply +)))

(comment
  ;; example 1
  (assert (= 142 (part1 exp1-input)))
  ;; part 1
  (assert (= 54450 (part1 part1-input)))
  ;; example 2
  (assert (= 281 (part2 exp2-input)))
  ;; part 2
  (assert (= 54265 (part2 part1-input))))


