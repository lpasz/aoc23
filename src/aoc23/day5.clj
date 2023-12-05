(ns aoc23.day5
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [core :as c]))

(def exp1-input (slurp "./inputs/day5/exp1.txt"))
(def part1-input (slurp "./inputs/day5/part1.txt"))

(defn in-between-range [[start range-size]]
  #(or (= start %) (< start % (+ start range-size))))

(defn in-between [[start1 start2 range-size]]
  [(in-between-range [start2 range-size])
   #(+ start1 (- % start2))
   (in-between-range [start1 range-size])
   #(+ start2 (- % start1))])

(defn to-funcs [lines]
  (->> (rest lines)
       (map #(re-seq #"\d+" %))
       (map (fn [ranges] (map biginteger ranges)))
       (map in-between)))

(defn to-ranges [lines]
  (->> (rest lines)
       (map #(re-seq #"\d+" %))
       (map (fn [ranges] (map biginteger ranges)))
       (map (fn [start]))))

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
    {:seeds (->> seeds (re-seq #"\d+") (map biginteger))
     :seed-to-soil (to-funcs seed-to-soil)
     :soil-to-fertilizer (to-funcs soil-to-fertilizer)
     :fertilizer-to-water (to-funcs fertilizer-to-water)
     :water-to-light (to-funcs water-to-light)
     :light-to-temperature (to-funcs light-to-temperature)
     :temperature-to-humidity (to-funcs temperature-to-humidity)
     :humidity-to-location (to-funcs humidity-to-location)}))

(parse-input part1-input)

(defn gets [coll-fn k dv]
  (or (->> coll-fn
           (filter (fn [[in-range _remaped-value]] (in-range k)))
           (keep (fn [[_in-range remaped-value]] (remaped-value k)))
           (first)) dv))

(defn gets-reversed [coll-fn k dv]
  (or (->> coll-fn
           (filter (fn [[_ _ in-range _]] (in-range k)))
           (keep (fn [[_ _ _ remaped-value]] (remaped-value k)))
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

(defn get-location-seed [location {:keys [seed-to-soil
                                          soil-to-fertilizer
                                          fertilizer-to-water
                                          water-to-light
                                          light-to-temperature
                                          temperature-to-humidity
                                          humidity-to-location]}]
  (let [humidity (gets-reversed humidity-to-location location location)
        temperature (gets-reversed temperature-to-humidity humidity humidity)
        light (gets-reversed light-to-temperature temperature temperature)
        water (gets-reversed water-to-light light light)
        fertilizer (gets-reversed fertilizer-to-water water water)
        soil (gets-reversed soil-to-fertilizer fertilizer fertilizer)
        seed (gets-reversed seed-to-soil soil soil)]
    seed))

(defn closest-spot [seeds almanaque]
  (reduce (fn [closest-location seed]
            (let [location (get-seed-location seed almanaque)]
              (if (nil? closest-location)
                location
                (min closest-location location))))
          nil
          seeds))

(defn part1 [inp]
  (let [almanaque (parse-input inp)]
    (closest-spot (:seeds almanaque) almanaque)))

(defn remap-seeds [almanaque]
  (-> almanaque
      (assoc :seeds-in-between-fns (->> (:seeds almanaque)
                                        (partition 2)
                                        (map in-between-range)))
      (assoc :seeds
             (->> (:seeds almanaque)
                  (partition 2)
                  (mapcat (fn [[s rng]] [s (+ s rng)]))))))

(defn part2 [inp range]
  (->> (parse-input inp)))

(part1 exp1-input)
(part1 part1-input)

(def almanaque (remap-seeds (parse-input part1-input)))
(def almanaque-exp (remap-seeds (parse-input exp1-input)))


(defn is-seed? [seed]
  (some #(% seed) (:seeds-in-between-fns almanaque-exp)))

(get-location-seed 32 almanaque-exp)

(def step 1000)

(def seeds 
  (->> )
  (lazy-cat
            (range 2637529854 (+ 2637529854 223394899) step)
            (range 3007537707 (+ 3007537707 503983167) step)
            (range 307349251 (+ 307349251 197383535) step)
            (range 3543757609 (+ 3543757609 276648400) step)
            (range 2296792159 (+ 2296792159 141010855) step)
            (range 116452725 (+ 116452725 5160533) step)
            (range 2246652813 (+ 2246652813 49767336) step)
            (range 762696372 (+ 762696372 160455077) step)
            (range 3960442213 (+ 3960442213 105867001) step)
            (range 1197133308 (+ 1197133308 38546766) step)))

;; jumping each 1000 we got that the smallest location found is 79874951
(assert (= 79875353 (closest-spot seeds almanaque)))
;; we get the seed for that location
(assert (= 3969172213N (get-location-seed 79875353 almanaque)))
;; with the location in hand we check around it, since we did not found any lower than 
(->> (for [seed (range (- 3969172213 20000) 3969172213)]
       [seed (get-seed-location seed almanaque)])
     (sort-by second))


(get-seed-location 3969171811 almanaque)




116452725 ;; lowest seed
3960442213 ;; highest seed

