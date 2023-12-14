(ns aoc23.day14
  (:require [core :as c]
            [clojure.string :as str]))


(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def directions
  {:north (fn [[x y]] [x (dec y)])
   :south (fn [[x y]] [x (inc y)])
   :west (fn [[x y]] [(dec x) y])
   :east (fn [[x y]] [(inc x) y])})

(defn go-direction [curr-coord dir mtx]
  (let [next-coord ((dir directions) curr-coord)]
    (case (get mtx next-coord)
      \. next-coord
      curr-coord)))

(defn move [mtx dir]
  (->> mtx
       (c/filter-by-value #{\O})
       (keys)
       (reduce (fn [acc-mtx round-rock-coord]
                 (let [next-round-rock-coord (go-direction round-rock-coord dir mtx)]
                   (if (= next-round-rock-coord round-rock-coord)
                     acc-mtx
                     (-> acc-mtx
                         (assoc next-round-rock-coord \O)
                         (assoc round-rock-coord \.)))))
               mtx)))

(defn tilt [dir mtx]
  (loop [mtx-acc mtx]
    (let [next-mtx (move mtx-acc dir)]
      (if (= next-mtx mtx-acc)
        mtx-acc
        (recur next-mtx)))))

(defn calculate-load-on-north [mtx]
  (let [max-y (inc (c/max-by #(-> % first second) mtx))]
    (->> mtx
         (c/filter-by-value #{\O})
         (map #(-> % first second))
         (map #(- max-y %))
         (reduce +))))

(def improvements (atom {}))
(def repeating (atom {}))

(defn tilt-cycle [dirs times mtx]
  (reset! improvements {})
  (reset! repeating {})
  (->> (range times)
       (reduce (fn [acc-mtx turn]
                 (let [load (calculate-load-on-north acc-mtx)]
                   (if-let [fturn (@improvements load)]
                     (when (not (@repeating fturn))
                       (swap! repeating assoc fturn turn))
                     (swap! improvements assoc load turn))
                   (reduce (fn [acc-mtx dir] (tilt dir acc-mtx)) acc-mtx dirs)))
               mtx)))

(defn calc-last-appearance [first-appear appears-each]
  (loop [n 1]
    (if (>= 1000000000 (+ (* n appears-each) first-appear))
      (recur (inc n))
      (+ first-appear (* (dec n) 7)))))

(defn calc-from-stored []
  (let [[fturn sturn] (last @repeating)
        last-apper (calc-last-appearance fturn (- sturn fturn))]
    (+ (- 1000000000 last-apper) fturn)))


(defn generate-small-store [inp n]
  (->> (c/to-matrix inp)
       (tilt-cycle [:north :west :south :east] n)))

(defn part1 [inp]
  (->> (c/to-matrix inp)
       (tilt :north)
       (calculate-load-on-north)))

(defn part2 [inp]
  (generate-small-store inp 130)
  (->> (calc-from-stored)
       (generate-small-store inp)
       (calculate-load-on-north)))

(comment
  (assert (= 136 (part1 exp1-input)))
  (assert (= 109665 (part1 part1-input)))
  (assert (= 64 (part2 exp1-input)))
  ;; super slow
  (assert (= 96061 (part2 part1-input)))
  ;;
  )