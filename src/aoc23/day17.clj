(ns aoc23.day17
  "Clumsy Crucible"
  (:require [core :as c]
            [complex.core :as cc]
            [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def j (cc/complex 0 1))
(def -j (cc/complex 0 -1))

(defn to-complex-matrix [inp]
  (->> (for [[idx row] (map-indexed (fn [idx row] [idx row]) (str/split-lines inp))]
         (for [[idy column] (map-indexed (fn [idy column] [idy (c/parse-int column)]) (re-seq #"\d" row))]
           [(cc/+ idx (cc/* idy (cc/complex 0 1))) column]))
       (c/flatten-once)
       (into {})))

(defn recalc [x y dx dy heat-loss deep min max mtx]
  (let [coord (cc/complex x y)
        dir (cc/complex dx dy)]
    (->> (for [next-dir [(cc// j dir) (cc// -j dir)]]
           (for [i (range min (inc max))
                 :let [next-coord (cc/+ coord (cc/* next-dir i))]
                 :when (contains? mtx next-coord)]
             [(cc/real-part next-coord)
              (cc/imaginary-part next-coord)
              (cc/real-part next-dir)
              (cc/imaginary-part next-dir)
              (+ heat-loss (->> (range 1 (inc i))
                                (map #(get mtx (cc/+ coord (cc/* next-dir %))))
                                (reduce +)))
              (inc deep)]))
         (flatten)
         (partition 6))))

(defn dijkstra [min max end mtx]
  (loop [queue (priority-map [0 0 0.0 0.0 1.0 0.0]
                             [0 0 0.0 0.0 1.0 0.0]
                             [0 0 0.0 0.0 1.0 0.0]
                             [0 0 0.0 0.0 1.0 0.0])
         seen #{}]
    (when-let [[[heat-loss deep x y dx dy] _] (first queue)]
      (cond (= [x y] end) heat-loss
            (seen [x y dx dy]) (recur (pop queue) seen)
            :else (recur (reduce (fn [acc [x y dx dy heat-loss deep]]
                                   (assoc acc [heat-loss deep x y dx dy] [heat-loss deep x y dx dy]))
                                 (dissoc queue [heat-loss deep x y dx dy])
                                 (recalc x y dx dy heat-loss deep min max mtx))
                         (conj seen [x y dx dy]))))))

(defn part1 [inp end-at]
  (->> inp
       (to-complex-matrix)
       (dijkstra 1 3 end-at)))

(defn part2 [inp end-at]
  (->> inp
       (to-complex-matrix)
       (dijkstra 4 10 end-at)))

(comment
  (assert (= 102 (part1 exp1-input [12.0 12.0])))
  (assert (= 742 (part1 part1-input [140.0 140.0])))
  (assert (= 94 (part2 exp1-input [12.0 12.0])))
  (assert (= 918 (part2 part1-input [140.0 140.0])))
  ;;
  )
