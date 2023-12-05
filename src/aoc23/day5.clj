(ns aoc23.day5
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [core :as c]))

(def exp1-input (slurp "./inputs/day5/exp1.txt"))
(def part1-input (slurp "./inputs/day5/part1.txt"))

(defn in-between [[start1 start2 range-size]]
  [#(or (= start2 %) (< start2 % (+ start2 range-size)))
   #(+ start1 (- % start2))])

(defn to-funcs [lines]
  (->> (rest lines)
       (map #(re-seq #"\d+" %))
       (map (fn [ranges] (map bigdec ranges)))
       (map in-between)))


;; (let [[is-in-range get-value] (in-between [52 50 48])]
;;   [(is-in-range 79)
;;    (get-value 79)])

(defn parse-input [inp]
  (let [[[seeds]
         seed-to-soil
         soil-to-fertilizer
         fertilizer-to-water
         water-to-light
         light-to-temperature
         temperature-to-humidity
         humidity-to-location] (->> (str/split inp #"\n\n")
                                    (map #(str/split-lines %)))]
    {:seeds (->> seeds (re-seq #"\d+") (map bigdec))
     :seed-to-soil (to-funcs seed-to-soil)
     :soil-to-fertilizer (to-funcs soil-to-fertilizer)
     :fertilizer-to-water (to-funcs fertilizer-to-water)
     :water-to-light (to-funcs water-to-light)
     :light-to-temperature (to-funcs light-to-temperature)
     :temperature-to-humidity (to-funcs temperature-to-humidity)
     :humidity-to-location (to-funcs humidity-to-location)}))

(defn gets [coll-fn k dv]
  (or (->> coll-fn
           (filter (fn [[in-range _remaped-value]] (in-range k)))
           (keep (fn [[_in-range remaped-value]] (remaped-value k)))
           (first)) dv))

(defn get-seed-location [seed {:keys [seed-to-soil
                                      soil-to-fertilizer
                                      fertilizer-to-water
                                      water-to-light
                                      light-to-temperature
                                      temperature-to-humidity
                                      humidity-to-location]}]
  (let [soil (gets seed-to-soil seed seed)
        fertilizer (gets soil-to-fertilizer soil soil)
        water (gets fertilizer-to-water fertilizer fertilizer)
        light (gets water-to-light water water)
        temperature (gets light-to-temperature light light)
        humidity (gets temperature-to-humidity temperature temperature)
        location (gets humidity-to-location humidity humidity)]
    location))

(defn almanaque-seeds [almanaque]
  (for [seed (:seeds almanaque)]
    (get-seed-location seed almanaque)))

(defn part1 [inp]
  (->> (parse-input inp)
       (almanaque-seeds)
       (apply min)))

(defn remap-seeds [almanaque]
  (assoc almanaque :seeds (lazy-cat
                           (->> (:seeds almanaque)
                                (partition 2)
                                (mapcat (fn [[s rng]]
                                          (range s (+ s rng))))))))

(defn closest-spot [almanaque]
  (reduce (fn [closest-location seed]
            (let [location (get-seed-location seed almanaque)]
              (if (nil? closest-location)
                location
                (min closest-location location))))
          nil
          (:seeds almanaque)))

(defn part2 [inp]
  (->> (parse-input inp)
       (remap-seeds)
       (closest-spot)))

(part1 exp1-input)
(part1 part1-input)


(part2 exp1-input)
(part2 part1-input)