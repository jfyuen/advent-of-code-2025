(ns day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file [filename process-line]
  (try
    (with-open [rdr (io/reader filename)]
      (process-line (line-seq rdr)))
    (catch java.io.FileNotFoundException e
      (println "Error: File not found -" filename))))


(defn parse-line [lines]
  (->> lines
       (map (fn [line]
              (map (fn [c] (Character/digit c 10)) (seq line))))))

(defn compute-joltage [numbers size]
  (let [bank      (vec numbers)
        splitted (- (count bank) (- size 1))
        available (subvec bank 0 splitted)
        res (subvec bank splitted)]
    (->> (concat res [0])
         (reduce (fn [[avail digits] next-item]
                   (let [max-val (apply max avail)
                         idx     (.indexOf avail max-val)]
                     [(conj (subvec avail (inc idx)) next-item)
                      (conj digits max-val)]))
                 [available []])
         (second)
         (apply str)
         (Long/parseLong))))


(defn -main [& args]
  (let [filename (first args)]
    (read-file filename
               (fn [lines]
                 (->> lines
                      (parse-line)
                      ;;(map (fn [l] (compute-joltage l 2)))
                      (map (fn [l] (compute-joltage l 12)))
                      (reduce +)
                      (println "---> "))))))