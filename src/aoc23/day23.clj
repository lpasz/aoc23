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

(defn parse [inp]
  (c/to-matrix inp))

(defn parse-f [inp]
  (->> (c/to-matrix inp)
       (c/reject #(= \# (val %)))
       (keys)
       (sort)))

(defn still-one-way? [curr-coord prev-coord pset]
  (->> (c/up-down-left-right curr-coord)
       (c/reject #{prev-coord})
       (filter pset)
       (count)
       (c/one?)))

(defn shortcuts [inp]
  (let [p (parse-f inp)
        pset (set p)
        psort-x (sort-by first p)
        psort-y (sort-by second p)
        [colapsed-x _ _] (reduce (fn [[acc [sx sy] [px py] cnt] [cx cy]]
                                   (if (and (= sx px cx)
                                            (= 1 (abs (- py cy)))
                                            (still-one-way? [cx cy] [px py] pset))
                                     [acc [sx sy] [cx cy] (inc cnt)]
                                     [(assoc acc [sx sy] [[px py] cnt] [px py] [[sx sy] cnt]) [cx cy] [cx cy] 0]))
                                 [(sorted-map) nil nil 0] psort-x)
        [colapsed-y _ _] (reduce (fn [[acc [sx sy] [px py] cnt] [cx cy]]
                                   (if (and (= sy py cy)
                                            (= 1 (abs (- px cx)))
                                            (still-one-way? [cx cy] [px py] pset))
                                     [acc [sx sy] [cx cy] (inc cnt)]
                                     [(assoc acc [sx sy] [[px py] cnt] [px py] [[sx sy] cnt]) [cx cy] [cx cy] 0]))
                                 [(sorted-map) nil nil 0] psort-y)]
    [(->> colapsed-x
          (c/reject (fn [[k [v _]]] (= k v)))
          (into (sorted-map)))
     (->> colapsed-y
          (c/reject (fn [[k [v _]]] (= k v)))
          (into (sorted-map)))]))

(defn get-shortcut [[sx sy] coord ccnt prev]
  (let [[curr [next-coord cnt]] (c/insp (case prev
                                  :x (when-let [short (get sy coord)] [:y short])
                                  :y (when-let [short (get sx coord)] [:x short])
                                  nil (let [shorty (get sy coord)
                                            shortx (get sx coord)]
                                        (cond (and (some? shorty)
                                                   (some? shortx)) (if (> (second shorty) (second shortx))
                                                                     [:y shorty]
                                                                     [:x shortx])
                                              shorty [:y shorty]
                                              shortx [:x shortx]
                                              :else nil))))]
    (when next-coord
      (or (get-shortcut [sx sy] next-coord (+ ccnt cnt) curr)
          [next-coord (+ ccnt cnt)]))))

(get-shortcut (shortcuts exp1-input) [1 0] 0 nil)

(defn next-paths [[coord ccnt visited] mtx shortcuts rule]
  (c/insp coord)
  (let [visited (conj visited coord)
        next-coords (->> (c/directions coord)
                         (keep (fn [[dir coord]] (when (rule [dir (get mtx coord)]) coord)))
                         (filter mtx)
                         (c/reject visited))]
    (if (c/one? (count next-coords))
      (if-let [[next-coord cnt] (get-shortcut shortcuts (first next-coords) 0 nil)]
        [[next-coord (+ cnt ccnt) visited]]
        (map (fn [coord] [coord (inc ccnt) visited]) next-coords))
      (map (fn [coord] [coord (inc ccnt) visited]) next-coords))))

(defn longest-path [mtx ends-at curr-coord shortcuts rule]
  (loop [paths #{[curr-coord 0 #{}]}
         cnt 0]
    (if (empty? paths)
      cnt
      (let [[curr-coord ccnt _visited :as curr] (first paths)
            rest-paths (disj paths curr)]
        (if (= curr-coord ends-at)
          (recur rest-paths (max cnt ccnt))
          (let [next-paths (->> (next-paths curr mtx shortcuts rule)
                                (reduce conj rest-paths))]
            (recur next-paths cnt)))))))


(longest-path (parse exp1-input) [21 22] [0 1] (shortcuts exp1-input) icy-valid-moves)
(longest-path (parse part1-input) [139 140] [0 1] (shortcuts part1-input)  icy-valid-moves)
(longest-path (parse exp1-input) [21 22] [0 1] valid-moves)
(longest-path (parse part1-input) [139 140] [0 1] valid-moves)

