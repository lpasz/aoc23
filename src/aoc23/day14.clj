(ns aoc23.day14
  (:require [core :as c]
            [clojure.string :as str]))


(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn whatsup [[x y] mtx]
  (case (get mtx [x (dec y)])
    \. [x (dec y)]
    [x y]))

(defn move-north [mtx]
  (->> mtx
       (c/filter-by-value #{\O})
       (keys)
       (reduce (fn [acc-mtx round-rock-coord]
                 (let [next-round-rock-coord (whatsup round-rock-coord mtx)]
                   (if (= next-round-rock-coord round-rock-coord)
                     acc-mtx
                     (-> acc-mtx
                         (assoc next-round-rock-coord \O)
                         (assoc round-rock-coord \.)))))
               mtx)))

(defn tilt-north [mtx]
  (loop [mtx-acc mtx]
    (let [next-mtx (move-north mtx-acc)]
      (if (= next-mtx mtx-acc)
        mtx-acc
        (recur next-mtx)))))

(defn part1 [inp]
  (->> (c/to-matrix inp)
       (tilt-north)
       (c/then [mtx] (let [max-y (inc (c/max-by #(-> % first second) mtx))]
                       (->> mtx
                            (c/filter-by-value #{\O})
                            (map #(-> % first second))
                            (map #(- max-y %))
                            (reduce +))))))



(assert (= 136 (part1 exp1-input)))
(assert (= 109665 (part1 part1-input)))