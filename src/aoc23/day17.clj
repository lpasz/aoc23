(ns aoc23.day17
  "Clumsy Crucible"
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def directions {:up #{:-> :<-}
                 :down #{:-> :<-}
                 :-> #{:up :down}
                 :<- #{:up :down}})

(defn- next-coord [x y n dir]
  (case dir
    :up   [x (- y n)]
    :down [x (+ y n)]
    :->   [(+ x n) y]
    :<-   [(- x n) y]))

(defn- recalc [[heat-loss deep x y dir] min max mtx]
  (->> (for [ndir (dir directions)]
         (for [i (range min (inc max))
               :let [ncoord  (next-coord x y i ndir)]
               :when (and (not= ncoord [x y]) (contains? mtx ncoord))]
           [(+ heat-loss (->> (range 1 (inc i))
                              (map #(next-coord x y % ndir))
                              (map #(get mtx %))
                              (c/sum)))
            (inc deep)
            ncoord
            ndir]))
       (flatten)
       (partition 5)
       (map vec)))

(defn- dijkstra [min max end mtx]
  (loop [queue (sorted-set [0 0 0 0 :->] [0 0 0 0 :down])
         seen #{}]
    (when-let [[heat-loss _deep x y dir :as args] (first queue)]
      (cond (= [x y] end) heat-loss
            (seen [x y dir]) (recur (disj queue args) seen)
            :else (recur (reduce conj (disj queue args) (recalc args min max mtx))
                         (conj seen [x y dir]))))))

(defn part1 [inp end-at]
  (->> (c/to-matrix inp #(re-seq #"\d" %) c/parse-int)
       (dijkstra 1 3 end-at)))

(defn part2 [inp end-at]
  (->> (c/to-matrix inp #(re-seq #"\d" %) c/parse-int)
       (dijkstra 4 10 end-at)))

(comment
  (assert (= 102 (part1 exp1-input [12 12])))
  (assert (= 742 (part1 part1-input [140 140])))
  (assert (= 94 (part2 exp1-input [12 12])))
  (assert (= 918 (part2 part1-input [140 140])))
  ;;
  )

