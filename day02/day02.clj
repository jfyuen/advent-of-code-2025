(ns day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file [filename process-line]
  (try
    (with-open [rdr (io/reader filename)]
      (process-line (line-seq rdr)))
    (catch java.io.FileNotFoundException e
      (println "Error: File not found -" filename))))


(defn parse-line [line]
  (let [splitted (str/split line #",")]
    (map (fn [s] (let [[_ start-str end-str] (re-find #"(\d+)-(\d+)" s)
                       start (Long/parseLong start-str)
                       end (Long/parseLong end-str)]
                   [start end])) splitted)))


(defn is-symetric1 [n]
  (let [s (str n)
        len (count s)]
    (if (even? len)
      (let [mid (/ len 2)
            part1 (subs s 0 mid)
            part2 (subs s mid)]
        (if (= part1 part2) n 0))
      0)))

(defn is-symetric2 [n]
  (let [s (str n)]
    (if (re-matches #"(\d+)\1+" s) n 0)))

(defn find-symmetric-numbers [a b]
  (map is-symetric2 (range a (inc b))))

(defn -main [& args]
  (let [filename (first args)]
    (read-file filename
               (fn [lines]
                 (let [numbers (mapcat (fn [item]
                                         (let [[start end] item]
                                           (find-symmetric-numbers start end)))
                                       (mapcat parse-line lines))]

                   (println " -> " (reduce + numbers)))))))
