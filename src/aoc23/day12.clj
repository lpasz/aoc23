(ns aoc23.day12
  (:require [core :as c]
            [clojure.string :as str]))

(def exp1-input (c/get-input "exp1.txt"))
(def part1-input (c/get-input "part1.txt"))

(defn parse-input [inp]
  (->> (str/split-lines inp)
       (map #(str/split % #" "))
       (map (fn [[spring-data rec-data]]
              [spring-data (->> rec-data
                                (re-seq #"\d+")
                                (map c/parse-int))]))))

(defn part2-fy [[text recovery] n]
  [(str/join "?" (repeat n text))
   (flatten (repeat n recovery))])

(def dfs-spring-pattern
  (memoize (fn  [springs pattern curr-spring-cnt]
             (cond
               ;; If it's both springs and pattern are empty and current current count is zero -> pattern matched
               (and (empty? springs) (empty? pattern) (zero? curr-spring-cnt)) 1
               ;; if springs is empty and pattern is the last current -> pattern matched
               (and (empty? springs) (= [curr-spring-cnt] pattern))  1
               ;; otherwise recursivelly continue or abandon
               :else (+
                      (if (#{\? \#} (first springs))
                        ;; is spring or ? continue as if is spring
                        (dfs-spring-pattern (rest springs) pattern (inc curr-spring-cnt))
                        0)
                      ;; is dot or ? continue as if is dot
                      (if (and (#{\. \?} (first springs))
                               (#{0 (first pattern)} curr-spring-cnt))
                        (if (zero? curr-spring-cnt)
                          ;; if current spring count is zero, just keep going
                          (dfs-spring-pattern (rest springs) pattern 0)
                          ;; if current spring count > zero, reset it, because we had a dot
                          (dfs-spring-pattern (rest springs) (rest pattern) 0))
                        0))))))

(defn part1 [inp]
  (->> (parse-input inp)
       (pmap #(dfs-spring-pattern (first %) (second %) 0))
       (reduce +)))

(defn part2 [inp]
  (->> (parse-input inp)
       (pmap #(part2-fy % 5))
       (pmap #(dfs-spring-pattern (first %) (second %) 0))
       (reduce +)))

(comment
  (assert (= 21 (part1 exp1-input)))
  (assert (= 7939 (part1 part1-input)))
  (assert (= 525152 (part2 exp1-input)))
  (assert (= 850504257483930 (part2 part1-input)))
  ;;
  )