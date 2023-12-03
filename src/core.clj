(ns core)

(defn map-key [fun coll]
  (map (fn [[key value]] [(fun key) value]) coll))

(defn max-by [fun coll]
  (apply max (map fun coll)))