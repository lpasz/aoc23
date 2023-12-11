(ns aoc23.day11
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn parse-input [inp]
  (->> (str/split-lines inp)
       (map seq)))

(defn all-space? [line]
  (every? #(= \. (second %)) line))

(defn euclidean-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn distance-from-galaxy-to-all-others [coords]
  (->> coords
       (reduce (fn [acc coord]
                 (reduce #(if (and (not= coord %2) (not (contains? %1 #{coord %2})))
                            (assoc %1 #{coord %2} (euclidean-distance coord %2))
                            %1)
                         acc
                         coords))
               {})))

(defn to-mtx-with-coords [inp]
  (map-indexed (fn [idy line] (map-indexed (fn [idx itm] [[idx idy] itm]) line)) inp))

(defn remap-coord [x jmp n]
  (+ (* jmp (dec n)) x))

(defn remap-coords [jmp n [[x y] itm] axis]
  (axis {:y [[x (remap-coord y jmp n)] itm]
         :x [[(remap-coord x jmp n) y] itm]}))

(defn expand [n axis mtx]
  (->> mtx
       (reduce (fn [[jmp acc] line]
                 (if (all-space? line)
                   [(inc jmp) (conj acc  line)]
                   [jmp (conj acc (map #(remap-coords jmp n % axis) line))]))
               [0 []])
       (second)))

(defn sum-galaxy-distances [inp empty-space-multiplier]
  (->> (parse-input inp)
       (to-mtx-with-coords)
       (expand empty-space-multiplier :y)
       (c/transpose)
       (expand empty-space-multiplier :x)
       (c/transpose)
       (c/flatten-once)
       (filter #(= \# (second %)))
       (into {})
       (keys)
       (distance-from-galaxy-to-all-others)
       (vals)
       (reduce +)))

(defn part1 [inp]
  (sum-galaxy-distances inp 2))

(defn part2 [inp]
  (sum-galaxy-distances inp 1000000))

(comment
  (assert (= 374 (part1 exp1-input)))
  (assert (= 9648398 (part1 part1-input)))
  (assert (= 1030 (sum-galaxy-distances exp1-input 10)))
  (assert (= 8410 (sum-galaxy-distances exp1-input 100)))
  (assert (= 618800410814 (part2 part1-input)))
;;
  )
