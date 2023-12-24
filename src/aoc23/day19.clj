(ns aoc23.day19
  "Aplenty"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn parse-items [items]
  (->> (str/split-lines items)
       (map (fn [line] (str/replace line #"[x|m|a|s]\=" #(str ":" (first %1) " "))))
       (map read-string)))

(defn left-right [[cnd left & right]]
  (if (= 1 (count right))
    [cnd left (first right)]
    [cnd left (left-right right)]))

(defn parse-workflows [workflows]
  (->> (str/split-lines workflows)
       (map #(re-seq #"([a-z]+)\{(.*)\}" %))
       (map (fn [[[_all key text]]] [key (left-right (str/split text #"\:|,"))]))
       (into {})))

(defn parse-input [inp]
  (let [[workflows items] (str/split inp  #"\n\n")]
    [(parse-workflows workflows) (parse-items items)]))

(defn find-valid-paths [root rng path]
  (when root
    (cond
      (#{"A"} root) [path]
      (#{"R"} root) []
      (vector? root) (concat (find-valid-paths (second root)
                                               rng
                                               (conj path [true (first root)]))
                             (find-valid-paths (nth root 2)
                                               rng
                                               (conj path [false (first root)])))
      (string? root)  (find-valid-paths (rng root) rng path))))


(defn do-not-empty [fun val coll]
  (if (empty? coll)
    val
    (apply fun coll)))

(defn invert-right-operation [sig]
  ({"<" ">=" ">" "<="} sig))


(defn calculate-valid-path-ranges [path]
  (->> path
       (map (fn [[left? text]] (let [[[_all chr sig num]] (re-seq #"(\w)(\<|\>)(\d+)"  text)
                                     op (if left? sig (invert-right-operation sig))
                                     v (c/parse-int num)]
                                 [(keyword chr) op (cond (= "<" op) (dec v)
                                                         (= ">" op) (inc v)
                                                         :else v)])))
       (group-by first)
       (map (fn [[k v]] [k  (map rest v)]))
       (map (fn [[key vs]]
              (let  [v (group-by first vs)
                     lower (->> (concat (get v "<" [])
                                        (get v "<=" []))
                                (map last)
                                (do-not-empty min 4000))
                     bigger (->> (concat (get v ">" [])
                                         (get v ">=" []))
                                 (map last)
                                 (do-not-empty max 1))]
                [key (list (min lower bigger) (max lower bigger))])))
       (into {:x '(1 4000) :m '(1 4000) :a '(1 4000) :s '(1 4000)})))


(defn any-valid-path-in-rng [{:keys [x m a s]} valid-path-rng]
  (some (fn [[[xmn xmx] [mmn mmx] [amn amx] [smn smx]]]
          (and (<= xmn x xmx) (<= mmn m mmx) (<= amn a amx) (<= smn s smx)))
        (map vals valid-path-rng)))

(defn part1 [inp]
  (let [[workflow items] (parse-input inp)
        valid-paths (find-valid-paths "in" workflow [])
        valid-path-rng (map calculate-valid-path-ranges valid-paths)]
    (->> items
         (filter #(any-valid-path-in-rng % valid-path-rng))
         (mapcat vals)
         (reduce +))))

(defn part2 [inp]
  (->> (parse-input inp)
       (first)
       (c/then [workflow] (find-valid-paths "in" workflow []))
       (map calculate-valid-path-ranges)
       (map vals)
       (map #(reduce (fn [acc [lo hi]] (* acc (inc (- hi  lo)))) 1 %))
       (reduce +)))

(comment
  (assert (= 19114 (part1 exp1-input)))
  (assert (= 575412 (part1 part1-input)))
  (assert (= 167409079868000 (part2 exp1-input)))
  (assert (= 126107942006821 (part2 part1-input)))
  ;;
  )