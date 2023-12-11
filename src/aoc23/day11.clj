(ns aoc23.day11
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp1-expanded-input (c/get-input "exp1-expanded.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn parse-input [inp]
  (->> (str/split-lines inp)
       (map seq)))

(defn all-space? [line]
  (every? #(= \. %) line))

(defn expand-lines [n coll]
  (mapcat (fn [line] (if (all-space? line) (repeat n line) [line]))
          coll))

(defn expand-space [n coll]
  (->> coll
       (expand-lines n)
       (c/transpose)
       (expand-lines n)
       (c/transpose)))

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

(defn part [inp n]
  (->> (parse-input inp)
       (expand-space n)
       (c/to-matrix)
       (filter #(= \# (second %)))
       (keys)
    ;;    (distance-from-galaxy-to-all-others)
    ;;    (vals)
    ;;    (sort)))
       ))

(defn part1 [inp]
  (part inp 2))

;; (defn part2 [inp]
;;   (part inp 1000000))


;; (assert (= 374 (part1 exp1-input)))
;; (assert (= 9648398 (part1 part1-input)))

;; (assert (= 1030 (part exp1-input 10)))
;; (assert (= 8410 (part exp1-input 100)))

(quot 1000000 100)

(defn a [n]
  (map (fn [x y] #(- y x n %)) (part exp1-input 1)  (part exp1-input n)))

(a 3)

'(4 4 4 4 4 5 5 5 5 5 6 7 7 7 7 7 7 7 8 8 8 8 9 9 10 10 11 11 11 12 12 12 12 13 13 15)
'(13 13 13 13 14 14 14 14 14 22 24 25 25 25 25 25 25 25 26 26 28 35 35 36 36 37 38 38 38 39 39 39 48 49 49 51)
'(103 103 103 103 104 104 104 104 104 202 204 205 205 205 205 205 205 205 206 206 208 305 305 306 306 307 308 308 308 309 309 309 408 409 409 411)
'(1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 3 3 3 3 3 3 3 4 4 4 4)

(+ 15 (* 4 99))

(defn diff [coll1 coll2]
  (map #(abs (- %1 %2)) coll1 coll2))

(diff (part exp1-input 1)
      (part exp1-input 10))
(part exp1-input 100)


(defn all-space-kv? [line]
  (every? #(= \. (second %)) line))


(defn to-mtx-with-coords [inp]
  (map-indexed (fn [idy line] (map-indexed (fn [idx itm] [[idx idy] itm]) line)) inp))

(defn expand-y [n mtx]
  (reduce (fn [[jmp acc] line]
            (if (all-space-kv? line)
              [(inc jmp) (conj acc  line)]
              [jmp (conj acc (map (fn [[[x y] itm]] [[x (+ (* jmp n) y)] itm]) line))]))
          [0 []]
          mtx))

(defn expand-x [n mtx]
  (reduce (fn [[jmp acc] line]
            (if (all-space? line)
              [(inc jmp) (conj acc line)]
              [jmp (conj acc (map (fn [[[x y] itm]]
                                    [[(+ (* jmp n) x) y] itm]) line))]))
          [0 []]
          mtx))

(defn part2 [inp n]
  (->> (parse-input inp)
       (to-mtx-with-coords)
       (expand-y n)
       (second)
       (c/flatten-once)
       (filter #(= \# (second %)))
       (into {})
       (keys)))
    ;;    (c/insp)
    ;;    (c/transpose)
    ;;    (expand-x n)
    ;;    (second)
    ;;    (c/transpose)
    ;;    (c/insp)
    ;;    (c/flatten-once)
    ;;    (filter #(= \# (second %)))
    ;;    (into {})
    ;;    (keys)))


(sort (part exp1-input 10))
(sort (part2 exp1-input 10))
