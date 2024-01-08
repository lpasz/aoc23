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
       (c/then [m]
               (merge-with into (c/revert-map m #{}) m))
       (map (fn [[key vals]] [key (vec vals)]))
       (into {})))

(parse exp1-input)

(defn contraction
  "Contract a edge between two nodes
   A - C - D   >-contract->     AC - D
   |           >-contract->     |
   B           >-contract->     B
   "
  [m n1 n2]
  (let [cluster-name n1
        e1 (get m n1)
        e2 (get m n2)
        cluster-edges (c/rejectv #{n1 n2} (into e1 e2))]
    (-> m
        (assoc cluster-name cluster-edges)
        (dissoc n2))))

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
    (c/insp g)
    (if (= 2 (count g))
      g
      (let [edge-start (rand-nth (keys g))
            edge-end (rand-nth (g edge-start))
            g (contraction g edge-start edge-end)]
        (recur g)))))


(krager node1)

