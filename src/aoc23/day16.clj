(ns aoc23.day16
  "The Floor Will Be Lava"
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(def start-coords-and-dir [[-1 0] "➡️"])

(defn- next-coord-by-dir [[[x y] dir]]
  (case dir
    "⬆"  [x (dec y)]
    "⬇️"  [x (inc y)]
    "➡️"  [(inc x) y]
    "⬅️"  [(dec x) y]))

(defn- next-coord-and-dir [mtx [coord dir]]
  (let [next-coord (next-coord-by-dir [coord dir])]
    (when-let [itm (get mtx next-coord)]
      (case [itm dir]
        [\| "➡️"] [[next-coord "⬆"] [next-coord "⬇️"]]
        [\| "⬅️"] [[next-coord "⬆"] [next-coord "⬇️"]]
        [\- "⬆"] [[next-coord "⬅️"] [next-coord "➡️"]]
        [\- "⬇️"] [[next-coord "⬅️"] [next-coord "➡️"]]
        [\\ "⬆"] [[next-coord "⬅️"]]
        [\\ "⬇️"] [[next-coord "➡️"]]
        [\\ "⬅️"] [[next-coord "⬆"]]
        [\\ "➡️"] [[next-coord "⬇️"]]
        [\/ "⬆"] [[next-coord "➡️"]]
        [\/ "⬇️"] [[next-coord "⬅️"]]
        [\/ "⬅️"] [[next-coord "⬇️"]]
        [\/ "➡️"] [[next-coord "⬆"]]
        [[next-coord dir]]))))

(defn- new-next-coords [mtx beam energized]
  (->> (next-coord-and-dir mtx beam)
       (c/reject energized)))

(defn- move [start-coords-and-dir mtx]
  (loop [beams [start-coords-and-dir]
         energized-coords #{}
         energized-coords-and-dir #{}]
    (if (empty? beams)
      energized-coords
      (let [beam (first beams)
            nexts (new-next-coords mtx beam energized-coords-and-dir)
            energized-coords-and-dir (conj energized-coords-and-dir beam)
            energized-coords (conj energized-coords (first beam))
            beams (reduce conj (rest beams) nexts)]
        (recur beams energized-coords energized-coords-and-dir)))))

(defn- generate-borders-up-down [n]
  (for [x (range n)]
    (for [[y dir] [[-1 "⬇️"] [n "⬆"]]]
      [[x y] dir])))

(defn- generate-borders-left-right [n]
  (for [x (range n)]
    (for [[y dir] [[-1 "➡️"] [n "⬅️"]]]
      [[y x] dir])))

(defn- edges [n]
  (c/flatten-once (concat (generate-borders-up-down n)
                          (generate-borders-left-right n))))

(defn part1
  ([inp] (part1 inp start-coords-and-dir))
  ([inp coords-and-dir]
   (->> (c/to-matrix inp)
        (move coords-and-dir)
        (count)
        (dec))))

(defn part2 [inp]
  (->> (str/split-lines inp)
       (count)
       (edges)
       (map #(part1 inp %))
       (reduce max)))

(comment
  (part1 exp1-input)
  (part1 part1-input)
  (part2 exp1-input)
  ;; a bit slow
  (part2 part1-input)
  ;;
  )