(ns day07
  (:require [clojure.string :as str]))

(defn count-splits [lines]
  (let [height (count lines)]

    (loop [y 0
           active-beams #{}
           total-splits 0]

      ;; stop recursion
      (if (>= y height)
        total-splits

        (let [row (nth lines y)
              current-beams (if (= 0 y)
                              (let [start-index (clojure.string/index-of row "S")] ;; first line, get the first beam location
                                (if start-index #{start-index} #{}))
                              active-beams)

              split-hits (filter (fn [x] (= \^ (get row x))) current-beams)
              pass-throughs (filter (fn [x] (not= \^ (get row x))) current-beams)
              new-from-splits (mapcat (fn [x] [(dec x) (inc x)]) split-hits)
              next-beams (set (concat pass-throughs new-from-splits))]

          (recur (inc y)
                 next-beams
                 (+ total-splits (count split-hits))))))))

(defn count-timelines [lines]
  (let [height (count lines)]

    (loop [y 0
           timelines {}]

      ;; stop here, sum all values on the accumulated timelines (rows)
      (if (>= y height)
        (reduce + (vals timelines))

        (let [row (nth lines y)
              current-row (if (= 0 y) ;; first line again
                            {(clojure.string/index-of row "S") 1}
                            timelines)

              accumulated-timelines
              (reduce-kv (fn [acc col count]
                           (let [current-char (get row col)]
                             (if (= current-char \^)
                               (-> acc
                                   (update (dec col) (fnil + 0) count)
                                   (update (inc col) (fnil + 0) count))

                               (update acc col (fnil + 0) count))))
                         {}
                         current-row)]

          (recur (inc y) accumulated-timelines))))))

;; Execute the function
(defn -main [& args]
  (let [lines (-> args first slurp str/split-lines)]
    (println "part1:" (count-splits lines))
    (println "part2:" (count-timelines lines))))
