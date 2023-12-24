(ns aoc23.day19
  "Aplenty"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(declare ^:dynamic x)
(declare ^:dynamic m)
(declare ^:dynamic a)
(declare ^:dynamic s)

(defn to-func
  [pred then & else]
  (let [[[_all variable conditional number]]  (re-seq #"([a-z])(\>|\<)(\d+)" pred)]
    (fn [mp]
      (binding [x (:x mp) m (:m mp) a (:a mp) s (:s mp)]
        (if (eval (read-string (str "(" conditional " " variable " " number ")")))
          then
          (if (= 1 (count else))
            (first else)
            ((apply to-func else) mp)))))))

(defn parse-workflows [workflows]
  (->> (str/split-lines workflows)
       (map #(re-seq #"([a-z]+)\{(.*)\}" %))
       (map (fn [[[_all key text]]] [key (apply to-func (str/split text #":|,"))]))
       (into {})))

(defn see-ranges [workflows]
  (->> (str/split-lines workflows)
       (map #(re-seq #"([a-z]+)\{(.*)\}" %))
       (mapcat (fn [[[_all key text]]] (map (fn [[_all s c n]] [s c (c/parse-int n)]) (re-seq #"([a-z])(\>|\<)(\d+)" text))))))

(see-ranges "px{a<2006:qkq,m>2090:A,rfg}\n")

(defn parse-items [items]
  (->> (str/split-lines items)
       (map (fn [line] (str/replace line #"[x|m|a|s]\=" #(str ":" (first %1) " "))))
       (map read-string)))

(defn parse-input [inp]
  (let [[workflows items] (str/split inp  #"\n\n")]
    [(parse-workflows workflows), (parse-items items) (see-ranges workflows)]))

(defn check-acceptance [item workflow]
  (loop [c "in"]
    (let [function (workflow c)
          result (function item)]
      (cond
        (#{"A"} result) true
        (#{"R"} result) false
        :else (recur result)))))


(defn left-right [[cnd left & right]]
  (if (= 1 (count right))
    [cnd left (first right)]
    [cnd left (left-right right)]))

(defn see-ranges [workflows]
  (->> (str/split-lines workflows)
       (map #(re-seq #"([a-z]+)\{(.*)\}" %))
       (map (fn [[[_all key text]]] [key (left-right (str/split text #"\:|,"))]))
       (into {})
       ;;
       ))

(defn find-valid-paths [root rng valid-path]
  (when root
    (cond
      (#{"A"} root) [valid-path]
      (#{"R"} root) []
      (vector? root) (concat (find-valid-paths (second root)
                                               rng
                                               (conj valid-path [true (first root)]))
                             (find-valid-paths (nth root 2)
                                               rng
                                               (conj valid-path [false (first root)])))
      (string? root) (let [[cnd left right] (rng root)]
                       (concat (find-valid-paths left
                                                 rng
                                                 (conj valid-path [true cnd]))
                               (find-valid-paths right
                                                 rng
                                                 (conj valid-path [false cnd])))))))

(defn do-if-not-empty [fun val coll]
  (if (empty? coll)
    val
    (apply fun coll)))

(defn calculate-valid-path-ranges [path]
  (->> path
       (map (fn [[left? text]] (let [[[_all chr sig num]] (re-seq #"(\w)(\<|\>)(\d+)"  text)
                                     op (if left? sig ({"<" ">" ">" "<"} sig))
                                     number (c/parse-int num)
                                     range-max (if (= "<" op)
                                                 (dec number)
                                                 (inc number))]
                                 [(keyword chr) range-max])))
       (group-by first)
       (map (fn [[k v]] [k  (map rest v)]))
       (map (fn [[key vs]]
              (let  [v (group-by first vs)
                     lower-than (->> (get v "<" [])
                                     (map last)
                                     (do-if-not-empty min 4000))
                     greater-than (->> (get v ">" [])
                                       (map last)
                                       (do-if-not-empty max 0))]
                [key (list  (inc (min lower-than greater-than)) (max lower-than greater-than))])))
       (into {:x '(1 4000) :m '(1 4000) :a '(1 4000) :s '(1 4000)})))


(defn part2 [inp]
  (->> (parse-input inp)
       (last)
       (c/then [rng] (find-valid-paths "in" rng []))
       (map calculate-valid-path-ranges)
       (map vals)
       (map #(reduce (fn [acc [lo hi]] (* acc (inc (- hi  lo)))) 1 %))
       (reduce +)))

(assert (= 167409079868000 (part2 exp1-input)))
(assert (= 126107942006821 (part2 part1-input)))
