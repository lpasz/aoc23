(ns aoc23.day5
  "If You Give A Seed A Fertilizer"
  (:require [clojure.string :as str]
            [core :as c]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn- in-between-range [[start range-size]]
  #(or (= start %) (< start % (+ start range-size))))

(defn- from-to [[start1 start2 range-size]]
  #(when ((in-between-range [start2 range-size]) %)
     (+ start1 (- % start2))))

(defn- to-from  [[start1 start2 range-size]]
  #(when ((in-between-range [start1 range-size]) %)
     (+ start2 (- % start1))))

(defn- to-funcs [lines from-to-fn]
  (->> (rest lines)
       (map #(re-seq #"\d+" %))
       (map (fn [nums] (map biginteger nums)))
       (map from-to-fn)))

(defn- parse-input [inp]
  (let [[[seeds]
         seed-to-soil
         soil-to-fertilizer
         fertilizer-to-water
         water-to-light
         light-to-temperature
         temperature-to-humidity
         humidity-to-location] (->> (str/split inp #"\n\n")
                                    (map #(str/split-lines %)))
        seeds (->> seeds (re-seq #"\d+") (map biginteger))]
    {:seeds seeds
     :seeds?-fns (->> (partition 2 seeds) (map in-between-range))
     :seeds-range  (->> (partition 2 seeds) (mapcat (fn [[s rng]] (range s (+ s rng)))))
     :seed-to-soil (to-funcs seed-to-soil from-to)
     :soil-to-fertilizer (to-funcs soil-to-fertilizer from-to)
     :fertilizer-to-water (to-funcs fertilizer-to-water from-to)
     :water-to-light (to-funcs water-to-light from-to)
     :light-to-temperature (to-funcs light-to-temperature from-to)
     :temperature-to-humidity (to-funcs temperature-to-humidity from-to)
     :humidity-to-location (to-funcs humidity-to-location from-to)
     :soil-to-seed (to-funcs seed-to-soil to-from)
     :fertilizer-to-soil (to-funcs soil-to-fertilizer to-from)
     :water-to-fertilizer (to-funcs fertilizer-to-water to-from)
     :light-to-water (to-funcs water-to-light to-from)
     :temperature-to-light (to-funcs light-to-temperature to-from)
     :humidity-to-temperature (to-funcs temperature-to-humidity to-from)
     :location-to-humidity (to-funcs humidity-to-location to-from)}))


(defn- gets [coll-fn k dv]
  (or (->> coll-fn (keep #(% k)) (first)) dv))

(defn- seed-to-location
  [seed
   {:keys [seed-to-soil
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

(defn- location-to-seed
  [location
   {:keys [soil-to-seed
           fertilizer-to-soil
           water-to-fertilizer
           light-to-water
           temperature-to-light
           humidity-to-temperature
           location-to-humidity]}]
  (let [humidity (gets location-to-humidity location location)
        temperature (gets humidity-to-temperature humidity humidity)
        light (gets temperature-to-light temperature temperature)
        water (gets light-to-water light light)
        fertilizer (gets water-to-fertilizer water water)
        soil (gets fertilizer-to-soil fertilizer fertilizer)
        seed (gets soil-to-seed soil soil)]
    seed))

(defn- keep-closest-location [closest-location seed almanaque]
  (let [location (seed-to-location seed almanaque)]
    (if-not (nil? closest-location)
      (min closest-location location)
      location)))

(defn- closest-spot [seeds almanaque]
  (reduce #(keep-closest-location %1 %2 almanaque) nil seeds))

(defn- seeds-with-steps [almanaque step]
  (->> (:seeds almanaque)
       (partition 2)
       (map (fn [[seed rng]] (range seed (+ seed rng) step)))
       (flatten)
       (lazy-cat)))

(defn part1 [inp]
  (c/->then (parse-input inp)
            #(closest-spot (:seeds %) %)))

(defn part2
  ([inp] (part2 inp 1))
  ([inp steps]
   (let [almanaque (parse-input inp)
         seeds (seeds-with-steps almanaque steps)
         close-location (closest-spot seeds almanaque)
         close-seed (location-to-seed close-location almanaque)]
     (->> (range (abs (- close-seed steps))
                 (inc (+ close-seed steps)))
          (map #(seed-to-location % almanaque))
          (sort)
          (first)))))

(assert (= 35 (part1 exp1-input)))
(assert (= 323142486 (part1 part1-input)))
(assert (= 46 (part2 exp1-input 1)))
(assert (= 79874951 (part2 part1-input 5000)))