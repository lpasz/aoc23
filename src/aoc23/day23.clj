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

(defn valid-moves [[_dir block]] (not (= \# block)))

(defn next-paths [mtx visited coord rule]
  (->> (c/directions coord)
       (keep (fn [[dir coord]] (when (rule [dir (get mtx coord)]) coord)))
       (filter mtx)
       (c/reject visited)
       (map (fn [coord] [coord visited]))))

(defn parse [inp]
  (c/to-matrix inp))

(defn longest-path [mtx ends-at curr-coord rule]
  (loop [paths #{[curr-coord #{}]}
         cnt 0]
    (if (empty? paths)
      cnt
      (let [[curr-coord visited :as curr] (first paths)
            rest-paths (disj paths curr)]
        (if (= curr-coord ends-at)
          (recur rest-paths (c/insp (max cnt (count visited))))
          (let [visited (conj visited curr-coord)
                next-paths (->> (next-paths mtx visited curr-coord rule)
                                (reduce conj rest-paths))]
            (recur next-paths cnt)))))))



(longest-path (parse exp1-input) [21 22] [0 1] icy-valid-moves)
(longest-path (parse part1-input) [139 140] [0 1] icy-valid-moves)
(longest-path (parse exp1-input) [21 22] [0 1] valid-moves)
(longest-path (parse part1-input) [139 140] [0 1] valid-moves)

