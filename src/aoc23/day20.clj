(ns aoc23.day20
  "Pulse Propagation"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp2-input (c/get-input "exp2.txt"))
(def part1-input (c/get-input "part1.txt"))

(def on 2r1)
(def off 2r0)

(defn flip-flop [pulse state]
  (bit-flip (bit-xor pulse state) 0))

(defn parse-positions [coll]
  (->> coll
       (map (fn [[key value]] [(re-find #"\w+" key) (re-seq #"\w+" value)]))
       (into {})))

(defn parse-state [coll]
  (->> coll
       (map (fn [[key _value]] [(re-find #"\w+" key) (re-find #"[%|&]" key)]))
       (into {})))

(defn parse-inp [inp]
  (->> (re-seq #"(?m)(.*) -> (.*)" inp)
       (map rest)
       (c/then [coll]
               {:position (parse-positions coll)
                :state (parse-state coll)})))



(defn revert-map [m]
  (reduce (fn [acc [k vs]]
            (reduce (fn [acc v]
                      (if (acc v)
                        (update acc v #(conj % k))
                        (assoc acc v [k]))) acc vs))
          {}
          m))


(defn run-cycle [start-at start-value position state]
  (let [go-to (position start-at)
        go-to-states]))

(let [{:keys [position state]} (parse-inp exp1-input)]
  (revert-map position)
  ;;
  )

(defn start-state-map [positions]
  (->> (cycle [off])
       (map (fn [k v] [k v]) (keys positions))
       (into {})))

(comment
  (start-state-map {"broadcaster" '("a" "b" "c"),
                    "a" '("b"),
                    "b" '("c"),
                    "c" '("inv"),
                    "inv" '("a")}))

(let [{:keys [position state]} (parse-inp exp1-input)
      low-pulses (atom 0)
      high-pulses (atom 0)
      current-state (atom (start-state-map position))])

;; p f    p f
;; 1 1 -> 1 1
;; 1 0 -> 0 0
;; 0 1 -> 0 0
;; 0 0 -> 1 1

