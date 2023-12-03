(ns core)

(defn map-key [fun coll]
  (map (fn [[key value]] [(fun key) value]) coll))

(defn filter-by-key [fun coll]
  (filter (fn [[key value]] [(fun key) value]) coll))

(defn max-by [fun coll]
  (apply max (map fun coll)))

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
  ([v] (clojure.pprint/pprint v) v)
  ([id v] (clojure.pprint/pprint {id v}) v))