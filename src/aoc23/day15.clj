(ns aoc23.day15
  "Lens Library"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- hash-calc [char hash-acc]
  (rem (* 17 (+ hash-acc (long char))) 256))

(defn- hash-str
  ([s] (hash-str s 0))
  ([[s1 & srest] hash]
   (if (nil? s1)
     hash
     (recur srest (hash-calc s1 hash)))))

(defn- add-key-hash-focal-or-update-focal [coll key hash-key focal]
  (if (some #(= key (first %)) coll)
    (map (fn [[k h f]] (if (= k key)
                         [k h  focal]
                         [k h f])) coll)
    (conj coll [key hash-key focal])))

(defn- remove-lens-with-key [coll key]
  (c/remove-first #(#{key} (first %)) coll))

(defn- parse-lens-box [lens-box]
  (let [[[_all key operation focal]] (re-seq #"([A-Z|a-z]+)([=|-])(\d+)?" lens-box)]
    [key
     operation
     (hash-str key)
     (c/parse-int focal)]))

(defn- hashmap [lens-boxes]
  (loop [lens-boxes lens-boxes
         acc {}]
    (if-let [lens-box (first lens-boxes)]
      (let [[key operation hash-key focal] (parse-lens-box lens-box)]
        (case operation
          "=" (recur (rest lens-boxes)
                     (update acc (hash-str key) #(add-key-hash-focal-or-update-focal % key hash-key focal)))
          "-" (recur (rest lens-boxes)
                     (update acc (hash-str key) #(remove-lens-with-key % key)))))
      acc)))

(defn part1 [inp]
  (->> (str/split inp #"\n|,")
       (map hash-str)
       (c/sum)))


(defn part2 [inp]
  (->> (str/split inp #"\n|,")
       (hashmap)
       ;; only the vals matter, we have the box value inside vals too
       (vals)
       ;; reverse because list insert at the start not at the end
       (map reverse)
       ;; calculate the focal power
       (mapcat #(map-indexed (fn [slot [_key box focal]]  (* (inc box) (inc slot) focal))  %))
       (c/sum)))

(comment
  (assert (= 1320 (part1 exp1-input)))
  (assert (= 517965 (part1 part1-input)))
  (assert (= 145 (part2 exp1-input)))
  (assert (= 267372 (part2 part1-input)))
  ;;
  )


