(ns aoc23.day10
  (:require [core :as c]
            [clojure.set :as set]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp2-input (c/get-input "exp2.txt"))
(def part1-input (c/get-input "part1.txt"))
(def exp3-input (c/get-input "exp3.txt"))
(def exp4-input (c/get-input "exp4.txt"))
(def exp5-input (c/get-input "exp5.txt"))

(defn find-start
  ([mtx] (find-start mtx \S))
  ([mtx value]
   (->> mtx
        (filter #(= value (val %)))
        (ffirst))))

(defn next-directions [[x y]]
  {:north [x (dec y)]
   :west [(dec x) y]
   :east [(inc x) y]
   :south [x (inc y)]})

(def moves {\| {:south :south :north :north};; is a vertical pipe connecting north and south.
            \- {:east :east :west :west};; is a horizontal pipe connecting east and west.
            \L {:south :east :west :north};; is a 90-degree bend connecting north and east.
            \J {:south :west :east :north};; is a 90-degree bend connecting north and west.
            \7 {:north :west :east :south};; is a 90-degree bend connecting south and west.
            \F {:north :east :west :south};; is a 90-degree bend connecting south and east.
            })



(defn two-in-same-pos [curr-pos]
  (first (reduce (fn [[repeated acc] [_direction value]]
                   (cond
                     (not (nil? repeated)) [repeated acc]
                     (acc value) [value acc]
                     :else [repeated (conj acc value)]))
                 [nil #{}]
                 curr-pos)))

(defn walk-pipe [start mtx]
  (loop [curr-pos (next-directions start)
         distance 1]
    (let [next-pos (->> curr-pos
                        (keep (fn [[direction position]]
                                (when-let [current-pipe (get mtx position)]
                                  (when-let [next-direction (get-in moves [current-pipe direction])]
                                    [next-direction (get (next-directions position) next-direction)])))))]
      (if (two-in-same-pos next-pos)
        (inc distance)
        (recur next-pos (inc distance))))))


(defn find-loop [mtx positions]
  (loop [current-possible-possitions (next-directions (first positions))
         positions positions]
    (let [[next-dir next-pos curr-pos] (->> current-possible-possitions
                                            (keep (fn [[direction position]]
                                                    (when-let [current-pipe (get mtx position)]
                                                      (when-let [next-direction (get-in moves [current-pipe direction])]
                                                        [next-direction
                                                         (get (next-directions position) next-direction)
                                                         position]))))
                                            (first))]
      (if ((set positions) next-pos)
        (conj positions curr-pos)
        (recur [[next-dir next-pos]]
               (conj positions next-pos))))))


(defn walk-one-step  [mtx [direction position]]
  (when-let [current-pipe (get mtx position)]
    (when-let [next-direction (get-in moves [current-pipe direction])]
      [next-direction
       (get (next-directions position) next-direction)])))

(defn next-valid-position [mtx position]
  (->> (next-directions position)
       (filter #(walk-one-step mtx %))
       (first)))

(defn find-loop-2 [mtx start]
  (let [next #(walk-one-step mtx %)
        [dir pos] (next-valid-position mtx start)]
    (loop [curr [dir pos]
           positions [start]]
      (let [[_direction position] curr]
        (if ((set positions) position)
          positions
          (recur (next curr)
                 (conj positions position)))))))


(defn shoelaces-area
  "See more: https://en.wikipedia.org/wiki/Shoelace_formula"
  [polygon-points]
  (->> polygon-points
       (partition 2 1)
       (map (fn [[[x1 y1] [x2 y2]]] (* (+ y1 y2) (- x1 x2))))
       (reduce +)
       (c/then [n] (quot n 2))
       (abs)))

(defn pick-internal-points
  "See more: https://en.wikipedia.org/wiki/Pick%27s_theorem"
  [area polygon-points]
  (- area (- (quot (count polygon-points) 2) 1)))


(defn part1 [inp]
  (->> (c/to-matrix inp)
       (c/then [mtx] (walk-pipe (find-start mtx) mtx))))

(defn part2 [inp]
  (let [mtx (c/to-matrix inp)
        loop-points (find-loop-2 mtx (find-start mtx))
        area (shoelaces-area loop-points)]
    (pick-internal-points area loop-points)))


(comment
  (assert (= 4 (part1 exp1-input)))
  (assert (= 8 (part1 exp2-input)))
  (assert (= 7005 (part1 part1-input)))
  (assert (= 1 (part2 exp1-input)))
  (assert (= 1 (part2 exp2-input)))
  (assert (= 4 (part2 exp3-input)))
  (assert (= 8 (part2 exp4-input)))
  (assert (= 417 (part2 part1-input)))
  ;;
  )


