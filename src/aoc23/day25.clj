(ns aoc23.day25
  "Snowverload"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- parse [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #"\:|\s"))
       (map (fn [[key _ & vals]] [key (set vals)]))
       (into {})
       (c/then [m] (merge-with into (c/revert-map m #{}) m))
       (map (fn [[key vals]] [key (vec vals)]))
       (into {})))

(defn contraction
  "Contract a edge between two nodes
   A - C - D   >-contract->     AC - D
   |           >-contract->     |
   B           >-contract->     B
   "
  [m n1 n2]
  (let [cluster-name (str n1 "-" n2)
        e1 (get m n1)
        e2 (get m n2)
        cluster-edges (c/rejectv #{n1 n2} (into e1 e2))
        m (-> m
              (assoc cluster-name cluster-edges)
              (dissoc n1)
              (dissoc n2))]
    (->> m
         (map (fn [[key vals]]
                [key (map #(get {n1 cluster-name n2 cluster-name} % %) vals)]))
         (into {}))))

;; A - B
;; |   |
;; C - D

(def node1
  {"a" ["b" "c"]
   "b" ["a" "d"]
   "c" ["a" "d"]
   "d" ["c" "b"]})

;; A - B
;; |   |
;; C - D
;; -----
;; A - B
;; | /
;; CD

(def node2
  {"a" ["b" "c" "d"]
   "b" ["a" "d"]
   "c" ["a" "d"]
   "d" ["c" "b" "a"]})

;; A - B
;; |   |
;; C - D
;; -----
;; A - B
;; | /
;; CD
(contraction node1 "d" "c")

;; A - B
;; | \ |
;; C - D
;; ------
;; A - B
;; || /
;; CD
(contraction node2 "d" "c")

(defn krager [g]
  (loop [g g]
    (if (= 2 (count g))
      (when (= 3 (count (first (vals g))))
        g)
      (let [edge-start (rand-nth (keys g))
            edge-end (rand-nth (g edge-start))
            g (contraction g edge-start edge-end)]
        (recur g)))))

(defn part1 [inp]
  (loop [g (parse inp)]
    (c/insp ".")
    (if-let [g-reduced (krager g)]
      (->> g-reduced
           (keys)
           (map #(str/split % #"-"))
           (map count)
           (reduce *))
      (recur g))))

(comment
  (assert (= 54 (part1 exp1-input)))
  (assert (= 543834 (part1 part1-input)))
  ;; there is no part 2 - it's done
  )