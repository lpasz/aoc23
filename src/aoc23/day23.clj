(ns aoc23.day23
  "A Long Walk"
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def icy-valid-moves #{[:up \^]
                       [:up \.]
                       [:down \v]
                       [:down \.]
                       [:-> \>]
                       [:-> \.]
                       [:<- \<]
                       [:<- \.]})

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

(defn nodes [inp]
  (let [mtx (->> (c/to-matrix inp)
                 (c/filter-by-value #(not (#{\#} %)))
                 (into (sorted-map)))]
    (->> mtx
         (keys)
         (filter #(< 2 (count (next-coords [% #{%}] mtx identity)))))))

(nodes exp1-input)

(defn distance-between-nodes [nodes mtx rule]
  (let [reach (conj (set nodes) [1 0] [21 22])]
    (concat (->> [[1 0] [21 22]]
                 (mapcat (fn [n] [n (into {} (next-paths [n #{n}] reach mtx rule))])))
            (->> nodes
                 (mapcat (fn [n]
                           [n (->> (next-paths [n #{n}] reach mtx rule)
                                   (mapcat (fn [nxt] (next-paths nxt reach mtx rule)))
                                   (map (fn [[key values]] [key (count values)])))]))))))



(distance-between-nodes (nodes exp1-input)
                        (parse exp1-input)
                        identity)

(defn longest-path [mtx ends-at curr-coord rule]
  (loop [paths  (c/queue [[curr-coord #{}]])
         cnt 0]
    (if (empty? paths)
      cnt
      (let [[curr-coord visited :as curr] (.peek paths)
            rest-paths (pop paths)]
        (if (ends-at curr-coord)
          (recur rest-paths (c/insp (max cnt (count visited))))
          (recur (->> (next-paths curr ends-at mtx rule)
                      (reduce conj rest-paths)) cnt))))))

(time (longest-path (parse exp1-input) #{[21 22]} [0 1] icy-valid-moves))
(time (longest-path (parse part1-input) #{[139 140]} [0 1] icy-valid-moves))
(time (longest-path (parse exp1-input) #{[21 22]} [1 0] valid-moves))
(time (longest-path (parse part1-input) #{[139 140]} [1 0] valid-moves))

