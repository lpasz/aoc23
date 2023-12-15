(ns aoc23.day15
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn hash-calc [char hash-acc]
  (rem (* 17 (+ hash-acc (long char))) 256))

(defn hash-str [s]
  (loop [s s h 0]
    (let [s1 (first s)
          srest (rest s)]
      (if (nil? s1)
        h
        (if (= s1 \newline)
          (recur srest h)
          (recur srest (hash-calc s1 h)))))))

(defn part1 [inp]
  (->> (str/split inp #"\n|,")
       (map hash-str)
       (reduce +)))

(defn add-key-hash-focal-or-update-focal [coll key hash-key focal]
  (if (some #(= key (first %)) coll)
    (map (fn [[k h f]] (if (= k key)
                         [k h  focal]
                         [k h f])) coll)
    (conj coll [key hash-key focal])))

(defn remove-lens-with-key [coll key]
  (c/remove-first #(#{key} (first %)) coll))

(defn hashmap [strs]
  (loop [strs strs
         acc (sorted-map)]
    (if (nil? (first strs))
      acc
      (let [[[_all key operation focal]] (->>  strs (first) (re-seq #"([A-Z|a-z]+)([=|-])(\d+)?"))
            hash-key (hash-str key)
            focal (c/parse-int focal)]
        (case operation
          "=" (recur (rest strs)
                     (update acc (hash-str key) #(add-key-hash-focal-or-update-focal % key hash-key focal)))
          "-" (recur (rest strs)
                     (update acc (hash-str key) #(remove-lens-with-key % key))))))))

(defn part2 [inp]
  (->> (str/split inp #"\n|,")
       (hashmap)
       (vals)
       (map reverse)
       (mapcat #(map-indexed (fn [slot [_key box focal]]  (* (inc box) (inc slot) focal))  %))
       (reduce +)))

(assert (= 1320 (part1 exp1-input)))
(assert (= 517965 (part1 part1-input)))
(assert (= 145 (part2 exp1-input)))
(assert (= 267372 (part2 part1-input)))


