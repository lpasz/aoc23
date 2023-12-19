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

(defn part1 [inp]
  (let [[workflows items] (parse-input inp)]
    (->> items
         (filter #(check-acceptance % workflows))
         (mapcat vals)
         (reduce +))))

(def step 500)
(def start 1)
(def end 4001)
(def wild (for [;;
                x (range start end step)
                m (range start end step)
                a (range start end step)
                s (range start end step)
                ;;
                ]
            {:x x
             :m m
             :a a
             :s s}))

(count wild)
(* 4000 4000 4000 4000)

;; (part1 exp1-input)
;; (part1 part1-input)

(defn part2 [inp]
  (let [[workflows _items] (parse-input inp)]
    (->> www
         (filter #(not (check-acceptance % workflows)))
         (partition 2 1)
         (map (fn [[{x1 :x m1 :m a1 :a s1 :s} {x2 :x m2 :m a2 :a s2 :s}]]
                (->> (for [x (range x1 (inc x2))]
                       (for [m (range m1 (inc m2))]
                         (for [a (range a1 (inc a2))]
                           (for [s (range s1 (inc s2))]
                             (+ x m a s)))))
                     (flatten)
                     (reduce +))))
         (reduce +))))

(seq {:x 2663, :m 2091, :a 3334, :s 2770})

(/ 18710016 12660538)
(+ 12660538
   6049478)


(reduce + (flatten (for [x (range 1 (inc 1))]
                     (for [m (range 1 (inc 1))]
                       (for [a (range 1 (inc 1))]
                         (for [s (range 1351 (inc 2770))]
                           (+ x m a s)))))))
(part2 exp1-input)

(->> '([:a 2006]
       [:a 2005]
       [:a 3333]
       [:a 1716]
       [:a 1717]
       [:a 3334]
       [:a 4000]
       [:m 2090]
       [:m 2091]
       [:m 1801]
       [:m 838]
       [:m 839]
       [:m 1548]
       [:m 1549]
       [:m 4000]
       [:m 1800]
       [:s 3448]
       [:s 1351]
       [:s 4000]
       [:s 2770]
       [:s 2771]
       [:s 536]
       [:s 3449]
       [:s 1350]
       [:s 537]
       [:x 2662]
       [:x 2441]
       [:x 1415]
       [:x 2440]
       [:x 4000]
       [:x 2663]
       [:x 1416])
     (group-by first)
     (map (fn [[k vs]] [k (let [vs (map second vs)]
                            (list (apply min vs) (apply max vs) (- (apply max vs) (apply min vs))))])))


(def mp (->> (parse-input exp1-input)
             (last)
             (group-by first)
             (mapcat (fn [[_ v]] (sort-by last v)))
             (reduce (fn [acc [symbol op num]]
                       (if (= op ">")
                         (update acc symbol #(conj % num (inc num)))
                         (update acc symbol #(conj % (dec num) num))))
                     {"x" [1]
                      "m" [1]
                      "a" [1]
                      "s" [1]})))

mp

(def www (flatten (for [x (conj (mp "x") 4000)]
                    (for [m (conj (mp "m") 4000)]
                      (for [a (conj (mp "a") 4000)]
                        (for [s (conj (mp "s") 4000)]
                          {:x (bigdec x) :m (bigdec m) :a (bigdec a) :s (bigdec s)}))))))

(* 4000 4000 4000 4000)

(Math/pow 4000 4)
(bigdec (reduce + [(Math/pow 2284 4)
                   (Math/pow 3162 4)
                   (Math/pow 3464 4)
                   (Math/pow 2585 4)]))





(- 256000000000000
   7349866944)


(- (count www) 816)
(* 2304/1488 (* 4000 4000 4000 4000))
(quot 12288000000000000 31)

