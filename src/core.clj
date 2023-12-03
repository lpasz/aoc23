(ns core)

(defn map-key [fun coll]
  (map (fn [[key value]] [(fun key) value]) coll))

(defn max-by [fun coll]
  (apply max (map fun coll)))

(defn flatten-once [coll]
  (mapcat identity coll))

(defn insp
  ([v] (clojure.pprint/pprint v) v)
  ([id v] (clojure.pprint/pprint {id v}) v))