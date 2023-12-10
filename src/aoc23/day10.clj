(ns aoc23.day10
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp2-input (c/get-input "exp2.txt"))
(def part1-input (c/get-input "part1.txt"))
(def exp3-input (c/get-input "exp3.txt"))
(def exp4-input (c/get-input "exp4.txt"))
(def exp5-input (c/get-input "exp5.txt"))

(defn- find-start [mtx]
  (->> mtx
       (filter #(= \S (val %)))
       (ffirst)))

(defn- next-directions [[x y]]
  {:north [x (dec y)] :west [(dec x) y] :east [(inc x) y] :south [x (inc y)]})

(def pipe-directions {\| {:south :south :north :north} ;; is a vertical pipe connecting north and south.
                      \- {:east :east :west :west} ;; is a horizontal pipe connecting east and west.
                      \L {:south :east :west :north} ;; is a 90-degree bend connecting north and east.
                      \J {:south :west :east :north} ;; is a 90-degree bend connecting north and west.
                      \7 {:north :west :east :south} ;; is a 90-degree bend connecting south and west.
                      \F {:north :east :west :south} ;; is a 90-degree bend connecting south and east.
                      })

(defn- walk-one-step [mtx [direction position]]
  (let [current-pipe (get mtx position)
        next-direction (get-in pipe-directions [current-pipe direction])
        next-position (get (next-directions position) next-direction)]
    (when (and current-pipe
               next-direction
               next-position)
      [next-direction next-position])))

(defn- find-next-valid-step
  "try to find a valid next step, and it's direction"
  [mtx position]
  (->> (next-directions position)
       (filter #(walk-one-step mtx %))
       (first)))

(defn- find-pipe-loop
  "get all position that form the pipe loop"
  [mtx start-position]
  (loop [curr-dir-pos (find-next-valid-step mtx start-position)
         positions [start-position]]
    (let [[_direction position] curr-dir-pos]
      (if (= start-position position)
        positions
        (recur (walk-one-step mtx curr-dir-pos)
               (conj positions position))))))


(defn- shoelaces-theorem-area
  "We are using the polygon version.
   See more: https://en.wikipedia.org/wiki/Shoelace_formula"
  [polygon-points]
  (->> polygon-points
       (partition 2 1)
       (map (fn [[[x1 y1] [x2 y2]]] (* (+ y1 y2) (- x1 x2))))
       (reduce +)
       (c/then [n] (quot n 2))
       (abs)))

(defn- pick-theorem-internal-points
  "Normaly used to find the area, since we found the area with shoelace, we are using it to get the number of internal points.
   See more: https://en.wikipedia.org/wiki/Pick%27s_theorem"
  [area polygon-points]
  (- area (- (quot (count polygon-points) 2) 1)))

(defn part1 [inp]
  (let [mtx (c/to-matrix inp)
        pipe-loop-points (find-pipe-loop mtx (find-start mtx))]
    (quot (count pipe-loop-points) 2)))

(defn part2 [inp]
  (let [mtx (c/to-matrix inp)
        pipe-loop-points (find-pipe-loop mtx (find-start mtx))
        pipe-loop-area (shoelaces-theorem-area pipe-loop-points)
        points-inside-loop (pick-theorem-internal-points pipe-loop-area pipe-loop-points)]
    points-inside-loop))

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
