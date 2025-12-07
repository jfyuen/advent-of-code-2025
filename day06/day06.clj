(ns day06
  (:require [clojure.string :as str]))

(defn transpose [coll]
  (apply mapv vector coll))

(defn read-operators [line]
  (map (fn [token]
         (case token
           "*" *
           "+" +))
       (str/split (str/trim line) #"\s+")))

(defn apply-ops [ops numbers]
  (map (fn [op col] (apply op col))
       ops
       numbers))

(defn resolve [lines grid-parser-fn]
  (let [grid-lines (butlast lines)
        op-line    (last lines)

        ops     (read-operators op-line)
        columns (grid-parser-fn grid-lines)]

    (apply-ops ops columns)))


(defn part1 [grid-lines]
  (let [rows (map (fn [line]
                    (->> (str/split (str/trim line) #"\s+")
                         (remove str/blank?)
                         (map parse-long)))
                  grid-lines)]
    (transpose rows)))


(defn part2 [grid-lines]
  (let [cols (transpose grid-lines)]
    (->> cols
         (map (fn [col-chars]
                (let [s (str/trim (str/join col-chars))]
                  (if (empty? s) nil (parse-long s)))))
         (partition-by nil?)
         (remove #(nil? (first %))))))

(defn -main [& args]
  (let [lines (-> args first slurp str/split-lines)]

    (->> (resolve lines part1)
         (reduce +)
         (println "> Part1: "))

    (->> (resolve lines part2)
         (reduce +)
         (println "> Part2: "))))