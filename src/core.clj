(ns core
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

;; create a `.clj-kondo/config.edn` and add the line below
;; {:lint-as {core/then clojure.core/fn}}
(defmacro then
  ([fun value] `(~fun ~value))
  ([args body value] `((fn ~args ~body) ~value)))

(defn fnvec [& funs]
  "create a function that receives a coll/vector and 
   returns a vector with first fn applied to first elem, second to second
   
   example: 
   ((fnvec inc dec core/parse-int) [0 3 \"3\"]) #=> [1 2 3]"
  (fn [args] (mapv (fn [fun args] (fun args)) funs args)))

(defn map-key [fun coll]
  (map (fn [[key value]] [(fun key) value]) coll))

(defn filter-by-key [fun coll]
  (filter (fn [[key _]] (fun key)) coll))

(defn filter-by-value [fun coll]
  (filter (fn [[_ value]] (fun value)) coll))

(defn max-by [fun coll]
  (apply max (map fun coll)))

(defn min-by [fun coll]
  (apply min (map fun coll)))

(defn flatten-once [coll]
  (mapcat identity coll))

(defn parse-int
  "Either parse the string to int, or return nil"
  [int]
  (try (Integer/parseInt int)
       (catch Exception e nil)))

(defn surrounding-xy [[x y]]
  [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
   [(dec x) y]                   [(inc x) y]
   [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]])

(defn insp
  ([v] (pp/pprint v) v)
  ([id v] (pp/pprint {id v}) v))

;; create a `.clj-kondo/config.edn` and add the line below
;; {:lint-as {core/then clojure.core/fn}}
(defmacro then
  ([fun value] `(~fun ~value))
  ([args body value] `((fn ~args ~body) ~value)))

(defmacro get-input [file]
  `(slurp (str "./inputs/"
               (-> ~*ns*
                   (ns-name)
                   (str)
                   (str/replace "aoc23." ""))
               "/"
               ~file)))

(defn to-matrix
  ([inp] (to-matrix inp identity identity))
  ([inp line-parse item-parse]
   (->> (if (string? inp)
          (str/split-lines inp)
          inp)
        (map-indexed (fn [idy line] (->> line line-parse (map-indexed (fn [idx c] [[idx idy] (item-parse c)])))))
        (flatten-once)
        (into (sorted-map)))))

(defn print-matrix [mtx]
  (println "")
  (->> mtx
       (group-by (comp second first))
       (into (sorted-map))
       (then [sor]
             (doseq [[_ lines] sor]
               (println (reduce str "" (map second lines))))))
  mtx)

(defn one? [n] (= 1 n))

(defn transpose [matrix]
  (apply map vector matrix))

(defn remove-first [pred coll]
  (lazy-seq
   (when (seq coll)
     (let [[y & ys] coll]
       (if (pred y)
         ys
         (cons y (remove-first pred ys)))))))

(defn reject [pred coll]
  (filter #(not (pred %)) coll))

(defn shoelaces-formula-area
  "We are using the polygon version.
   See more: https://en.wikipedia.org/wiki/Shoelace_formula"
  [polygon-points]
  (->> polygon-points
       (partition 2 1)
       (map (fn [[[x1 y1] [x2 y2]]] (* (+ y1 y2) (- x1 x2))))
       (reduce +)
       (then [n] (quot n 2))
       (abs)))

(defn pick-theorem-internal-points
  "Normaly used to find the area, since we found the area with shoelace, we are using it to get the number of internal points.
   See more: https://en.wikipedia.org/wiki/Pick%27s_theorem"
  [area polygon-points-cnt]
  (- area (- (quot polygon-points-cnt 2) 1)))


(defn revert-map [m]
  (reduce (fn [acc [k vs]]
            (reduce (fn [acc v]
                      (if (acc v)
                        (update acc v #(conj % k))
                        (assoc acc v [k])))
                    acc
                    vs))
          {}
          m))