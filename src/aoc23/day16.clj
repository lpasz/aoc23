(ns aoc23.day16
  "The Floor Will Be Lava"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def start-coords-and-dir [[-1 0] :->])

(defn- next-coord-by-dir [[[x y] dir]]
  (case dir
    :up [x (dec y)]
    :down [x (inc y)]
    :->  [(inc x) y]
    :<-  [(dec x) y]))

(defn- next-coord-and-dir [mtx [coord dir]]
  (let [next-coord (next-coord-by-dir [coord dir])]
    (when-let [itm (get mtx next-coord)]
      (cond
        (and (= \| itm) (#{:<- :->} dir)) [[next-coord :up] [next-coord :down]]
        (and (= \- itm) (#{:up :down} dir)) [[next-coord :<-] [next-coord :->]]
        (and (= \\ itm) (= :up dir)) [[next-coord :<-]]
        (and (= \\ itm) (= :down dir)) [[next-coord :->]]
        (and (= \\ itm) (= :<- dir)) [[next-coord :up]]
        (and (= \\ itm) (= :-> dir)) [[next-coord :down]]
        (and (= \/ itm) (= :up dir)) [[next-coord :->]]
        (and (= \/ itm) (= :down dir)) [[next-coord :<-]]
        (and (= \/ itm) (= :<- dir)) [[next-coord :down]]
        (and (= \/ itm) (= :-> dir)) [[next-coord :up]]
        :else [[next-coord dir]]))))

(defn- new-next-coords [mtx beam energized]
  (->> (next-coord-and-dir mtx beam)
       (filter #(not (energized %)))))

(defn- move [start-coords-and-dir mtx]
  (loop [beams [start-coords-and-dir]
         energized #{}]
    (if (empty? beams)
      energized
      (let [beam (first beams)
            nexts (new-next-coords mtx beam energized)
            energized (conj energized beam)
            beams (reduce conj (rest beams) nexts)]
        (recur beams energized)))))

(defn- edges [n]
  (->> (concat (for [x (range n)]
                 (for [[y dir] [[-1 :down] [n :up]]]
                   [[x y] dir]))
               (for [x (range n)]
                 (for [[y dir] [[-1 :->] [n :<-]]]
                   [[y x] dir])))
       (c/flatten-once)))

(defn part1
  ([inp]
   (part1 inp start-coords-and-dir))
  ([inp coords-and-dir]
   (->> (c/to-matrix inp)
        (move coords-and-dir)
        (map first)
        (into #{})
        (count)
        (dec))))

(defn part2 [inp]
  (->> (str/split-lines inp)
       (count)
       (edges)
       (reduce #(max %1 (part1 inp %2)))))

(comment
  (part1 exp1-input)
  (part1 part1-input)
  (part2 exp1-input)
  ;; a bit slow
  (part2 part1-input)
  ;;
  )