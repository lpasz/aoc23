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
  (->> (c/to-matrix inp)
       (c/reject #(= \# (val %)))
       (into (sorted-map))))

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
        [colapsed-y _ _] (reduce (fn [[acc [sx sy] [px py] visited] [cx cy]]
                                   (c/insp [[sx sy] [px py] [cx cy] visited])
                                   (if (and (= sx px cx)
                                            (= 1 (abs (- py cy)))
                                            (c/insp (still-one-way? [cx cy] [px py] pset)))
                                     [acc [sx sy] [cx cy] (conj visited [cx cy])]
                                     [(assoc acc
                                             [sx sy] [[cx cy] (conj visited [cx cy])]
                                             [cx cy] [[sx sy] (conj visited [cx cy])])
                                      [cx cy]
                                      [cx cy]
                                      #{[cx cy]}
                                      1]))
                                 [(sorted-map) nil nil #{}] psort-x)
        [colapsed-x _ _] (reduce (fn [[acc [sx sy] [px py] visited] [cx cy]]
                                   (if (and (= sy py cy)
                                            (= 1 (abs (- px cx)))
                                            (still-one-way? [cx cy] [px py] pset))
                                     [acc [sx sy] [cx cy] (conj visited [cx cy])]
                                     [(assoc acc [sx sy] [[px py] visited] [px py] [[sx sy] visited]) [cx cy] [cx cy] #{[cx cy]} 1]))
                                 [(sorted-map) nil nil] psort-y)]
    [(->> colapsed-x
          (c/reject (fn [[k [v _]]] (= k v)))
          (into (sorted-map)))
     (->> colapsed-y
          (c/reject (fn [[k [v _]]] (= k v)))
          (into (sorted-map)))]))


(defn to-shortcuts [coll]
  (->> coll
       (c/reject #(c/one? (count %)))
       (mapcat (fn [itm] (let [f (first itm)
                               l (last itm)
                               s (set itm)]
                           [[f [l s]] [l [f s]]])))
       (into (sorted-map))))

(defn same-x-next-y [[x1 y1] [x2 y2]] (and (= x1 x2)
                                           (= 1 (abs (- y1 y2)))))

(defn same-y-next-x  [[x1 y1] [x2 y2]] (and (= y1 y2)
                                            (= 1 (abs (- x1 x2)))))

(defn fork [set] (fn [[x1 y1] [x2 y2]]
  ())

(defn shortcuts2 [inp]
  (let [points (parse-f inp)
        points-set (set points)
        psort-x (sort-by first points)
        psort-y (sort-by second points)
        y-shortcuts (c/partition-while same-x-next-y psort-x)
        y-shortcuts (mapcat #(c/partition-while  %) y-shortcuts))
        x-shortcuts (c/partition-while same-y-next-x psort-y)]
    (to-shortcuts x-shortcuts)))

(shortcuts2 exp1-input)


(c/partition-while (fn [[x1 y1] [x2 y2]] (and (= x1 x2) (= 1 (abs (- y1 y2)))))
                   (sort-by first (parse-f exp1-input)))

(c/partition-while (fn [[x1 y1] [x2 y2]] (and (= y1 y2) (= 1 (abs (- x1 x2)))))
                   (sort-by second (parse-f exp1-input)))


(shortcuts exp1-input)
((parse exp1-input) [3 4])

(defn get-shortcut [[sx sy] coord visited prev]
  (let [[curr [next-coord nvisited]]   (case prev
                                         :x (when-let [short (get sy coord)] [:y short])
                                         :y (when-let [short (get sx coord)] [:x short])
                                         nil (let [shorty (get sy coord)
                                                   shortx (get sx coord)]
                                               (cond (and (some? shorty)
                                                          (some? shortx)) (if (> (count (nth shorty 1))
                                                                                 (count (nth shortx 1)))
                                                                            [:y shorty]
                                                                            [:x shortx])
                                                     shorty [:y shorty]
                                                     shortx [:x shortx]
                                                     :else nil)))]
    (when next-coord
      (or (get-shortcut [sx sy] next-coord  (reduce conj visited nvisited) curr)
          [next-coord (reduce conj visited nvisited)]))))

(get-shortcut (shortcuts exp1-input) [1 0] (sorted-set [1 0]) nil)

(defn next-coords [[coord visited] mtx rule]
  (->> (c/directions coord)
       (keep (fn [[dir coord]] (when (rule [dir (get mtx coord)]) coord)))
       (filter mtx)
       (c/reject visited)))

(next-coords [[3 5] #{[3 4]}] (parse exp1-input) icy-valid-moves)

(defn next-paths [[coord visited] mtx shortcuts rule]
  (let [visited (conj visited coord)
        next-coords (next-coords [coord visited] mtx rule)]
    (if (c/one? (count next-coords))
      (if-let [[next-coord nvisited] (get-shortcut shortcuts (first next-coords) visited nil)]
        [[next-coord nvisited]]
        (map (fn [coord] [coord visited]) next-coords))
      (map (fn [coord] [coord visited]) next-coords))))

(next-paths [[1 0] #{[1 0]}] (parse exp1-input) (shortcuts exp1-input) icy-valid-moves)

(defn longest-path [mtx ends-at curr-coord shortcuts rule]
  (loop [paths (c/queue [[curr-coord #{}]])
         cnt 0]
    (if (empty? paths)
      cnt
      (let [[curr-coord visited :as curr] (peek paths)
            rest-paths (pop paths)]
        (c/insp :curr-coord curr-coord)
        (if (= curr-coord ends-at)
          (recur rest-paths (max cnt (count visited)))
          (let [next-paths (next-paths curr mtx shortcuts rule)
                _ (c/insp :next-coord  (map first next-paths))]
            (recur  (reduce conj rest-paths next-paths) cnt)))))))


(longest-path (parse exp1-input) [21 22] [0 1] (shortcuts2 exp1-input) icy-valid-moves)
(longest-path (parse part1-input) [139 140] [0 1] (shortcuts part1-input)  icy-valid-moves)
(longest-path (parse exp1-input) [21 22] [0 1] (shortcuts exp1-input) identity)
(longest-path (parse part1-input) [139 140] [0 1] (shortcuts exp1-input) identity)

(defn buil)