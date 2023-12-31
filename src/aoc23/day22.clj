(ns aoc23.day22
  "Sand Slabs"
  (:require [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- rangex [p1 p2]
  (range (min p1 p2) (inc (max p1 p2))))

(defn- get-positions [[x1 y1 z1 x2 y2 z2]]
  (for [x (rangex x1 x2) y (rangex y1 y2) z (rangex z1 z2)]
    [x y z]))

(defn- parse-input [inp]
  (->> (re-seq #"(?m)(\d+),(\d+),(\d+)\~(\d+),(\d+),(\d+)" inp)
       (map rest)
       (map #(map parse-long %))
       (map-indexed (fn [idx v] [idx (get-positions v)]))))


(defn- step-down [brick-coords]
  (map (fn [[x y z]] [x y (dec z)]) brick-coords))

(defn- floor? [brick-coords] (some #(zero? (last %)) brick-coords))

(defn- next-brick-pos  [brick-coords occupied]
  (let [brick-down (step-down brick-coords)]
    (cond (floor? brick-down) brick-coords
          (some occupied brick-down) brick-coords
          :else (or (next-brick-pos brick-down occupied) brick-down))))

(defn- by-lowest-z
  "It will always start by lowest z, so we just get the first z coord"
  [[_id [[_x _y z]]]]
  z)

(defn- brick-fall
  "Make the bricks fall to it's final position"
  [bricks]
  (loop [bricks (sort-by by-lowest-z bricks)
         bricks-after-falling {}
         occupied {}]
    (if (empty? bricks)
      [bricks-after-falling occupied]
      (let [[id brick] (first bricks)
            bricks (rest bricks)
            brick-next (next-brick-pos brick occupied)
            bricks-after-falling (assoc bricks-after-falling id brick-next)
            occupied (reduce #(assoc %1 %2 id) occupied brick-next)]
        (recur bricks bricks-after-falling occupied)))))

(defn- supports-and-supported-by
  "We get a brick, and remove it's position from occupied (to avoid it hitting itself).
   After that we check if the brick can move down, and record what it hit's.
   If it hits something it means that this are the brick/bricks that serve as support for it.
   We revert this to get the the supported by."
  [bricks-after-fall occupied]
  (->> bricks-after-fall
       (reduce (fn [acc [id brick-coords]]
                 (let [occupied-by-others (reduce dissoc occupied brick-coords)
                       brick-down-pos (step-down brick-coords)
                       supported-by-bricks (set (keep occupied-by-others brick-down-pos))]
                   (if (empty? supported-by-bricks)
                     acc
                     (assoc acc id supported-by-bricks))))
               {})
       (c/then [supported-by-bricks]
               [(c/revert-map supported-by-bricks #{}) supported-by-bricks])))

(defn- check-if-would-fall
  "Get the bricks that the current bricks is giving support to.
   Then get the bricks that are giving support to those bricks.
   If only the current brick is giving support to that, we say that things would colapse."
  [[id _brick] support-bricks support-by-bricks]
  (->> (support-bricks id)
       (map support-by-bricks)
       (filter #(= #{id} %))
       (not-empty)))

(defn- fallable-bricks [bricks support-bricks support-by-bricks]
  (filter #(check-if-would-fall % support-bricks support-by-bricks) bricks))

(defn- cnt-bricks-falling
  "We need to add an id to the bricks, so we can keep track of what moved, an what not.
   Then we apply the same algorithm as before, and let them fall.
   We discard the ones that did not move and count the rest."
  [[id _desintegrated-brick] bricks]
  (let [remaning-bricks (dissoc bricks id)
        remaning-bricks-set (set remaning-bricks)
        [remaning-bricks-after-fall _]  (brick-fall remaning-bricks)]
    (->> remaning-bricks-after-fall
         (c/reject remaning-bricks-set)
         (count))))

(defn part1 [inp]
  (let [bricks (parse-input inp)
        [bricks-after-fall occupied] (brick-fall bricks)
        [support-bricks support-by-bricks]  (supports-and-supported-by bricks-after-fall occupied)
        all-bricks (count bricks-after-fall)
        fallable-bricks (count  (fallable-bricks bricks-after-fall support-bricks support-by-bricks))]
    (- all-bricks fallable-bricks)))

(defn part2 [inp]
  (let [bricks (parse-input inp)
        [bricks-after-fall occupied] (brick-fall bricks)
        [support-bricks support-by-bricks] (supports-and-supported-by bricks-after-fall occupied)
        fallable-bricks (fallable-bricks bricks-after-fall support-bricks support-by-bricks)]
    (->> fallable-bricks
         (map #(cnt-bricks-falling % bricks-after-fall))
         (c/sum))))

(comment
  (assert (= 5 (part1 exp1-input)))
  (assert (= 393 (part1 part1-input)))
  (assert (= 7 (part2 exp1-input)))
  (assert (= 58440 (part2 part1-input)))
  ;;
  )
