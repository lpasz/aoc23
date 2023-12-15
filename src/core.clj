(ns core
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

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

;; {:lint-as {core/then clojure.core/fn}} on your clj-kondo
(defmacro then
  "To use with ->> to avoid"
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

(defn to-matrix [inp]
  (->> (if (string? inp)
         (str/split-lines inp)
         inp)
       (map-indexed (fn [idy line] (->> line (map-indexed (fn [idx c] [[idx idy] c])))))
       (flatten-once)
       (into (sorted-map))))

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

(defn remove-first [x coll]
  (lazy-seq
   (when (seq coll)
     (let [[y & ys] coll]
       (if (x y)
         ys
         (cons y (remove-first x ys)))))))




