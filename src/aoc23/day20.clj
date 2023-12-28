(ns aoc23.day20
  "Pulse Propagation"
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp2-input (c/get-input "exp2.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- receive-from [coll]
  (->> coll
       (map (fn [[key value]] [(re-find #"\w+" key) (vec (re-seq #"\w+" value))]))
       (into {})
       (c/revert-map)))

(defn- parse [coll]
  (let [receives-from (receive-from coll)]
    (->> coll
         (map (fn [[key value]]
                (let [id (re-find #"\w+" key)]
                  [id
                   {:type (or (first (re-find #"[%|&]" key)) id)
                    :send-to (vec (re-seq #"\w+" value))
                    :receive-from (->> (receives-from id) (map (fn [k] [k false])) (into {}))
                    :memory false}])))
         (into {}))))


(defn- parse-inp [inp]
  (->> (re-seq #"(?m)(.*) -> (.*)" inp)
       (map rest)
       (parse)))

(defn- queue [coll]
  (reduce conj clojure.lang.PersistentQueue/EMPTY coll))

(defn update-part1 [part1 signal]
  (if (true? signal)
    (update part1 :high inc)
    (update part1 :low inc)))

(defn- press-button [[inp part1 part2] n]
  (loop [queue (queue [["button" false "broadcaster"]])
         inp inp
         part1 part1
         part2 part2]
    (if (empty? queue)
      [inp part1 part2]
      (let [[sender signal receiver] (peek queue)
            queue (pop queue)
            itm (inp receiver)
            part1 (update-part1 part1 signal)
            [send-signal inp] (case (:type itm nil)
                                \% (cond
                                     (true? signal) [nil inp]
                                     (:memory itm) [false (assoc-in inp [receiver :memory] false)]
                                     :else [true (assoc-in inp [receiver :memory] true)])
                                \& (let [inp (-> inp
                                                 (assoc-in [receiver :memory] signal)
                                                 (assoc-in [receiver :receive-from sender] signal))]
                                     [(->> (get-in inp [receiver :receive-from])
                                           (vals)
                                           (every? true?)
                                           (not))
                                      inp])
                                "broadcaster" [signal inp]
                                [nil inp])
            ;; _ (c/insp [receiver send-signal (:send-to itm)])
            send-to (->> (:send-to itm) (map (fn [send-to] [receiver send-signal send-to])))
            part2 (cond
                    (= ["ss" true "ql"] [sender signal receiver]) (assoc part2 "ss" n)
                    (= ["fz" true "ql"] [sender signal receiver]) (assoc part2 "fz" n)
                    (= ["fh" true "ql"] [sender signal receiver]) (assoc part2 "fh" n)
                    (= ["mf" true "ql"] [sender signal receiver]) (assoc part2 "mf" n)
                    :else part2)]
        (if (nil? send-signal)
          (recur queue inp part1 part2)
          (recur (reduce conj queue send-to)
                 inp
                 part1
                 part2))))))

(defn part1 [inp]
  (->> (range 1 (inc 1000))
       (reduce press-button [(parse-inp inp) {:high 0 :low 0} {}])
       (second)
       (vals)
       (reduce *)))

(defn part2 [inp]
  (->> (range 1 (inc 4000))
       (reduce press-button [(parse-inp inp) {:high 0 :low 0} {}])
       (last)
       (vals)
       (reduce *)))

(comment
  (assert (= 32000000 (part1 exp1-input)))
  (assert (= 11687500 (part1 exp2-input)))
  (assert (= 787056720 (part1 part1-input)))
  (assert (= 212986464842911 (part2 part1-input)))
  ;;
  )
