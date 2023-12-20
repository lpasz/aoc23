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
        (#{"A"} result) 1
        (#{"R"} result) 0
        :else (recur result)))))

(defn part1 [inp]
  (let [[workflows items] (parse-input inp)]
    (->> items
         (filter #(check-acceptance % workflows))
         (mapcat vals)
         (reduce +))))

(defn rng [x] (map (fn [x y] [x (- x y)]) (rest x) x))

(let [splits {"x" [0 4000]
              "m" [0 4000]
              "a" [0 4000]
              "s" [0 4000]}
      [workflows items] (doall (parse-input part1-input))]
  (->> (re-seq #"(?m)(\w+)(\<|\>)(\d+)" part1-input)
       (reduce (fn [splits [_all c op v]]
                 (update splits c #(conj % (- (c/parse-int v) (if (= op "<") 1 0)))))
               splits)
       (map (fn [[k v]]  (rng (sort v))))
       (c/then [[x m a s]]
               (for [[x dx] x]
                 (for [[m dm] m]
                   (for [[a da] a]
                     (for [[s, ds] s]
                       (c/insp (* dx dm da ds 
                          (check-acceptance {:x x :m m :a a :s s} workflows))))))))
       (flatten)
       (reduce + (bigint 0))))
