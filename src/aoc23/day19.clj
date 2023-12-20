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

(defn some-try [root rng path path2]
  (when root
    (cond
      (#{"A"} root) [{:path path :steps path2}] ;;[{:path path :steps path2}]
      (#{"R"} root)  [];; [{:path path :steps path2}]
      (vector? root) (concat (some-try (second root)
                              rng 
                              (conj path [true (first root)])
                              (conj path2 (second root)))
                          (some-try (nth root 2)
                                    rng
                                    (conj path [false (first root)])
                                    (conj path2 (nth root 2))))
      (string? root) (let [[cnd left right] (rng root)]
                       (concat (some-try left
                                         rng
                                         (conj path [true cnd]) 
                                         (conj path2 root))
                             (some-try right 
                                       rng
                                       (conj path [false cnd])
                                       (conj path2 root)))))))

(defn do-not-empty [fun val coll]
  (if (empty? coll)
    val
    (apply fun coll)))

(defn do-path-calc [path]
  (->> path 
    ;;    (filter first)
       (map (fn [[left? text]] (let [[[_all chr sig num]] (re-seq #"(\w)(\<|\>)(\d+)"  text)]
                                    [(keyword chr)
                                     (if left? sig ({"<" ">" ">" "<"} sig)) 
                                     (c/parse-int num)])))
       (group-by first)
       (map (fn [[k v]] [k  (map rest v)]))
       (map (fn [[key vs]] (let  [v (group-by first vs)
                                  max (->> (get v "<" []) (map last) (do-not-empty max 4000)) 
                                  min (->> (get v ">" []) (map last) (do-not-empty min 1))]  
                           [key 
                            ;; {:max  max :min min }
                            (into #{} (range min (inc max)))
                            ;; (- max min)                            
                            ])))
       (into {})
    ;;    (merge {:x {}
    ;;            :m {}
    ;;            :a {}
    ;;            :s {}})
       ))

(let [x (->> (some-try "in" (last (parse-input exp1-input)) [] [])
             (map #(assoc % :path (into {} (do-path-calc (:path %)))))
             (map :path))]
  (->> (for [f [:x :m :a :s]]
         (clojure.set/difference (into #))(->> x
              (map f)
              (reduce clojure.set/union)
              ))))

(- (* 4000 4000 3999 2582) (* 2755 4000 2006 2581))


(* 4000 4000 3999 2621)
(->> (some-try "in" (last (parse-input exp1-input)) [] [])
     (map #(assoc % :path (do-path-calc (:path %))))
    ;;  (map :path)
    ;;  (group-by first)
    ;;  (map (fn [[k vs]]
    ;;         (let [mx (:max (first (sort-by :max > (mapcat rest vs)))) 
    ;;               mn  (:min (first (sort-by :min (mapcat rest vs))))]
    ;;         [k mx mn (- mx mn) (- (inc mx) mn)

    ;;          ;;
    ;;          ])))
)
(- 4000 (- 2770 1351))
          

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





4000 1 1 1

(* 4000 4000 4000)

(- (* 4000 4000 4000 4000) 
   177280000000000)

(let [[_wfls _itms rng] (parse-input inp)]
  (loop [[c l r] (rng "in")
         visited #{}]
    (let [[cl ll rl] (get rng l)
          [cr lr rr] (get rng r)]
    (cond))

    (defn- dijkstra [end mtx]
      (loop [queue [()]
             seen #{}]
        (when-let [[heat-loss _deep x y dir :as args] (first queue)]
          (cond (= [x y] end) heat-loss
                (seen [x y dir]) (recur (disj queue args) seen)
                :else (recur (reduce conj (disj queue args) (recalc args min max mtx))
                             (conj seen [x y dir]))))))


(defn part1 [inp]
  (let [[workflows items] (parse-input inp)]
    (->> items
         (filter #(check-acceptance % workflows))
         (mapcat vals)
         (reduce +))))


;; (part1 exp1-input)
;; (part1 part1-input))))))))))))))))))


