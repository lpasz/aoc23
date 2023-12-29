(ns aoc23.day21
  "Step Counter"
  (:require [core :as c]
            [clojure.set :as set]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn remap [n nmax]
  (let [rm (rem n nmax)]
    (cond (zero? rm) 0
          (<= 0 n nmax) n
          (> n nmax) rm
          (< n 0) (+ rm nmax))))

(defn remap-xy [[x y] nmax]
  [(remap x nmax) (remap y nmax)])

(assert (= (map #(remap % 11) (range -11  22))
           '(0 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 10 0 1 2 3 4 5 6 7 8 9 10)))

(defn gardens-around [coord mtx nmax]
  (->> (c/up-down-left-right coord)
       (c/reject #(#{\#} (get mtx (remap-xy % nmax))))
       (set)))

(def center-to-edge 65)
(def elf-steps 26501365)
(def max-visited-gardens (quot 26501365 131))

(defn part [inp n]
  (let [mtx (c/to-matrix inp)
        nmax (inc (reduce max (map first (keys mtx))))
        start-at (ffirst (filter (fn [[_key v]] (#{\S} v)) mtx))
        mtx (assoc mtx start-at \.)]
    (loop [state (sorted-map 0 #{start-at})
           results []]
      (if (empty? state)
        :ops
        (let [[curr-step coords] (first state)
              state (dissoc state curr-step)]
          (if (= curr-step n)
            [(count coords) results]
            (let [next-coords (reduce #(set/union %1 (gardens-around %2 mtx nmax)) #{} coords)]
              (recur (assoc state (inc curr-step) next-coords)
                     (if (= center-to-edge (rem curr-step nmax))
                       (conj results (count coords))
                       results)))))))))

(defonce abc (second (part part1-input 328)))

(defn calc [n a b c]
  (+ a (* n (+ (- b a) (quot (* (dec n) (+ a (- c b b))) 2)))))

(defn part1 [inp]
  (first (part inp 64)))

(defn part2 []
  (let [[a b c] abc]
    (calc max-visited-gardens a b c)))

(assert (= 16 (first (part exp1-input 6))))
(assert (= 3776 (part1 part1-input)))
(assert (= 625587097150084 (part2)))




   