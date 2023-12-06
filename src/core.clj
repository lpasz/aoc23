(ns core
  (:require [clojure.pprint :as pp]))

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
  [args body value]
  `((fn ~args ~body) ~value))