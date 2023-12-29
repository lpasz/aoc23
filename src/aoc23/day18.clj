(ns aoc23.day18
  "Lavaduct Lagoon"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- move [[x y] n dir]
  (case dir
    "U" [x (- y n)]
    "D" [x (+ y n)]
    "L" [(- x n) y]
    "R" [(+ x n) y]))

(def hex-to-dir {\0 "R" \1 "D" \2 "L" \3 "U"})

(defn- last-hex-to-dir [hex]
  (hex-to-dir (last hex)))

(defn first-5-hex-to-int [hex]
  (->> hex (take 5) (cons "16r") (str/join) (read-string)))

(defn- reinstruct [hex]
  ((juxt last-hex-to-dir first-5-hex-to-int)
   (->> (re-seq #"[0-9|a-z]+" hex)
        (first))))

(defn- parse-input [inp]
  (->> (str/split-lines inp)
       (map #(re-find #"(R|L|D|U) (\d+) (.*)" %))
       (map rest)
       (map (c/fnvec identity c/parse-int reinstruct))))

(defn- to-polygon-vertices [instructions]
  (pop (reduce (fn [acc [dir amount]] (conj acc (move (peek acc) amount dir))) [[0 0]] instructions)))

(defn- to-polygon-ext-pts-cnt [instructions]
  (->> instructions
       (map second)
       (reduce +)))

(defn- calc-lava-area [instructions]
  (let [polygon-edge-cnt (to-polygon-ext-pts-cnt instructions)
        polygon-vertices (to-polygon-vertices instructions)
        polygon-area (c/shoelaces-formula-area polygon-vertices)
        polygon-internal (c/pick-theorem-internal-points polygon-area polygon-edge-cnt)]
    (+ polygon-edge-cnt polygon-internal)))

(defn part1 [inp]
  (->> (parse-input inp)
       (calc-lava-area)))

(defn part2 [inp]
  (->> (parse-input inp)
       (map last)
       (calc-lava-area)))

(comment
  (assert (= 62 (part1 exp1-input)))
  (assert (= 62573 (part1 part1-input)))
  (assert (= 952408144115 (part2 exp1-input)))
  (assert (= 54662804037719 (part2 part1-input)))
  ;;
  )




