(ns aoc23.day23
  "A Long Walk"
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def icy-valid-moves #{[:up \^] [:up \.]
                       [:down \v] [:down \.]
                       [:-> \>] [:-> \.]
                       [:<- \<] [:<- \.]})

(def valid-moves identity)

(defn next-coords [[coord visited] mtx rule]
  (let [visited (conj visited coord)]
    (->> (c/directions coord)
         (keep (fn [[dir coord]] (when (rule [dir (get mtx coord)]) coord)))
         (filter mtx)
         (c/reject visited)
         (map (fn [coord] [coord visited])))))

(defn next-paths [curr ends-at mtx rule]
  (let [coords (next-coords curr mtx rule)]
    (if (and (c/one? (count coords))
             (not (ends-at (ffirst coords))))
      (recur (first coords) ends-at mtx rule)
      coords)))

(defn parse [inp]
  (->> (c/to-matrix inp)
       (c/filter-by-value #(not (#{\#} %)))
       (into {})))

(defn longest-path [mtx ends-at curr-coord rule]
  (loop [paths  (c/queue [[curr-coord #{}]])
         cnt 0]
    (if (empty? paths)
      cnt
      (let [[curr-coord visited :as curr] (.peek paths)
            rest-paths (pop paths)]
        (if (ends-at curr-coord)
          (recur rest-paths  (max cnt (count visited)))
          (recur (->> (next-paths curr ends-at mtx rule)
                      (reduce conj rest-paths)) cnt))))))

(defn nodes [inp]
  (let [mtx (->> (c/to-matrix inp)
                 (c/filter-by-value #(not (#{\#} %)))
                 (into (sorted-map)))]
    (->> mtx
         (keys)
         (filter #(< 2 (count (next-coords [% #{%}] mtx identity)))))))

(defn distance-between-nodes [nodes start-at end-at mtx rule]
  (let [reach (conj (set nodes) start-at end-at)
        start-end (->> [start-at end-at]
                       (mapcat (fn [n] [n (->> (next-paths [n #{n}] reach mtx rule)
                                               (map (fn [[key values]] [key (count values)]))
                                               (into {}))])))
        nodes (->> nodes
                   (mapcat (fn [n]
                             [n (->> (next-paths [n #{n}] reach mtx rule)
                                     (mapcat (fn [nxt] (next-paths nxt reach mtx rule)))
                                     (map (fn [[key values]] [key (count values)]))
                                     (into {}))])))]
    (->> (concat start-end nodes)
         (partition 2)
         (map vec)
         (into {}))))

(defn longest-path-nodes [graph start-at ends-at]
  (loop [currs #{[start-at 0 #{start-at}]}
         max-cost 0]
    (if (empty? currs)
      max-cost
      (let [[curr-coord cost visited] (first currs)
            rcurrs (disj currs [curr-coord cost visited])
            visited (conj visited curr-coord)]
        (if (= ends-at curr-coord)
          (recur rcurrs (max max-cost cost))
          (recur (->> (get graph curr-coord [])
                      (c/reject (fn [[key _]] (visited key)))
                      (reduce (fn [acc [ncoord ncost]]
                                (conj acc [ncoord (+ ncost cost) visited])) rcurrs))
                 max-cost))))))

(defn part1 [inp start-at ends-at]
  (-> (parse inp)
      (longest-path #{ends-at} start-at icy-valid-moves)))

(defn part2 [inp start-at ends-at]
  (-> (nodes inp)
      (distance-between-nodes start-at ends-at (parse inp) valid-moves)
      (longest-path-nodes start-at ends-at)))

(comment
  (assert (= 94 (time (part1 exp1-input [1 0] [21 22]))))
  (assert (= 2394 (time (part1 part1-input [1 0] [139 140]))))
  (assert (= 154 (time (part2 exp1-input [1 0] [21 22]))))
  ;; takes around 120s beware of running it
  (assert (= 6554 (time (part2 part1-input [1 0] [139 140]))))
  ;;
  )