(ns day04
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
              (map (fn [c] (if (= c \@) 1 0)) (seq line))))))
(def deltas [[[-1 -1] [-1 0] [-1 1]
              [0 -1]         [0 1]
              [1 -1] [1 0] [1 1]]])

(defn neighbor-sum [grid r c]
  (reduce +
          (map (fn [[dr dc]]
                 (get-in grid [(+ r dr) (+ c dc)] 0))
               deltas)))

;; part 1
(defn count-with-neighbors [grid]
  (let [grid (mapv vec grid)
        rows   (count grid)
        cols   (count (first grid))]
    (count
     (for [r (range rows)
           c (range cols)
           :let [current-val (get-in grid [r c])]

           :when (= 1 current-val)
           :let [neighbor-count (neighbor-sum grid r c)]
           :when (< neighbor-count 4)]
       true))))


;; part 2
(defn count-all-removed [init-grid]
  (let [rows (count init-grid)
        cols (count (first init-grid))]

    (loop [grid (mapv vec init-grid)
           total-removed 0]

      (let [to-remove (for [r (range rows)
                            c (range cols)
                            :let [val (get-in grid [r c])]
                            :when (= 1 val)

                            :let [total-neigbhours (neighbor-sum grid r c)]
                            :when (< total-neigbhours 4)]
                        [r c])]

        (if (empty? to-remove)
          total-removed
          (let [count-this-round (count to-remove)
                next-grid (reduce (fn [g [r c]]
                                    (assoc-in g [r c] 0))
                                  grid
                                  to-remove)]
            (recur next-grid
                   (+ total-removed count-this-round))))))))

(defn -main [& args]
  (let [filename (first args)]
    (read-file filename
               (fn [lines]
                 (->> lines
                      (parse-line)
                      (count-all-removed)
                      (println "> "))))))