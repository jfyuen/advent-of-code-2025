(ns day01
  (:require [clojure.java.io :as io]))

(defn read-file [filename process-line]
  (try
    (with-open [rdr (io/reader filename)]
        (process-line (line-seq rdr)))
    (catch java.io.FileNotFoundException e
      (println "Error: File not found -" filename))))

(defn parse-line [line]
  (if-let [[_ direction num] (re-find #"([RL])(\d+)" line)]
    (let [n (Integer/parseInt num)]
      (if (= direction "R") n (- n)))
    nil))

(defn next-number [start value]
  (let [total (+ start value)]
     (mod total 100)))


(defn count-zeros [numbers]
  (first
   (reduce (fn [[zeros current-val] n]
             (let [new-val (next-number current-val n)]
               [(if (= 0 new-val) (+ zeros 1) zeros)
                new-val]))
           [0 50]
           numbers)))

(defn -main [& args]
  (let [filename (first args)]
    (read-file filename
               (fn [lines]
                 (let [numbers (keep parse-line lines)
                       zeros (count-zeros numbers)]
                     (println "number of zeros:" zeros))))))
