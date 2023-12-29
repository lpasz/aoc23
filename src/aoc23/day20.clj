(ns aoc23.day20
  "Pulse Propagation"
  (:require [core :as c]
            [clojure.math.numeric-tower :as math]))

(def exp1-input (c/get-input "exp1.txt"))
(def exp2-input (c/get-input "exp2.txt"))
(def part1-input (c/get-input "part1.txt"))

(def high-pulse? true?)
(def low-pulse? false?)
(def pulse-propagated? #(not (nil? %)))

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
                    :send-to-modules (vec (re-seq #"\w+" value))
                    :receive-from (->> (receives-from id) (map (fn [k] [k false])) (into {}))
                    :memory false}])))
         (into {}))))


(defn- parse-inp [inp]
  (->> (re-seq #"(?m)(.*) -> (.*)" inp)
       (map rest)
       (parse)))

(defn update-part1 [part1 signal]
  (if (high-pulse? signal)
    (update part1 :high inc)
    (update part1 :low inc)))

(defn update-part2
  "We are looking at the input itself and determining what need to happen for us to actually trigger rx.
   In this case `&ql -> rx` and `&ss -> ql` `&fz -> ql` `&fh -> ql` `&mf -> ql`
   They happen all around 4000 pulses, we just need to keep track when, afterwards we multiply to get the largest commom denominator"
  [[sender signal receiver] part2 n]
  (cond
    (= ["ss" true "ql"] [sender signal receiver]) (assoc part2 "ss" n)
    (= ["fz" true "ql"] [sender signal receiver]) (assoc part2 "fz" n)
    (= ["fh" true "ql"] [sender signal receiver]) (assoc part2 "fh" n)
    (= ["mf" true "ql"] [sender signal receiver]) (assoc part2 "mf" n)
    :else part2))

(defn state-after-pulse [[sender signal receiver] {:keys [type memory]} state]
  (case type
    \% (if (low-pulse? signal)
         (assoc-in state [receiver :memory] (not memory))
         state)
    \& (-> state
           (assoc-in [receiver :memory] signal)
           (assoc-in [receiver :receive-from sender] signal))
    state))

(defn signal-to-send [[_sender signal receiver] {:keys [type memory]} state]
  (case type
    \% (when (low-pulse? signal) (not memory))
    \& (->> (get-in state [receiver :receive-from])
            (vals)
            (every? high-pulse?)
            (not))
    "broadcaster" signal
    nil))

(defn send-to-modules [[_ _ sender] {:keys [send-to-modules]} signal]
  (map (fn [receiver] [sender signal receiver]) send-to-modules))

(defn- press-button [[state part1 part2] n]
  (loop [queue (c/queue [["button" false "broadcaster"]])
         state state
         part1 part1
         part2 part2]
    (if (empty? queue)
      [state part1 part2]
      (let [[_sender signal receiver :as pulse] (peek queue)
            queue (pop queue)
            module (state receiver)
            part1 (update-part1 part1 signal)
            part2 (update-part2 pulse part2 n)
            state (state-after-pulse pulse module state)
            send-signal (signal-to-send pulse module state)
            send-to-modules (send-to-modules pulse module send-signal)]
        (if (pulse-propagated? send-signal)
          (recur (reduce conj queue send-to-modules) state part1 part2)
          (recur queue state part1 part2))))))

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
       (reduce math/lcm)))

(comment
  (assert (= 32000000 (part1 exp1-input)))
  (assert (= 11687500 (part1 exp2-input)))
  (assert (= 787056720 (part1 part1-input)))
  (assert (= 212986464842911 (part2 part1-input)))
  ;;
  )