(ns day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-file [filename]
  (let [content (slurp filename)
        [ranges values] (str/split content #"\n\n")]

    {:ranges
     (vec (for [line (str/split-lines ranges)]
            (let [[min-s max-s] (str/split line #"-")]
              [(Long/parseLong min-s) (Long/parseLong max-s)])))

     :values
     (vec (for [line (str/split-lines values)]
            (Long/parseLong line)))}))

;; part 1
(defn in-any-range? [val ranges]
  (boolean
   (some (fn [[r-min r-max]]
           (<= r-min val r-max))
         ranges)))

(defn check-values [data]
  (reduce + 0 (map (fn [v] (if (in-any-range? v (:ranges data)) 1 0)) (:values data))))

;; part2
(defn merge-ranges [ranges]
  (if (empty? ranges)
    []
    (let [sorted-ranges (sort-by first ranges)]

      (reduce (fn [acc [curr-start curr-end]]
                (if (empty? acc)
                  [[curr-start curr-end]]
                  (let [[last-start last-end] (peek acc)]
                    (if (<= curr-start last-end)
                      (assoc acc (dec (count acc))
                             [last-start (max last-end curr-end)])
                      (conj acc [curr-start curr-end])))))
              []
              sorted-ranges))))

(defn count-in-ranges [raw-ranges]
  (let [merged     (merge-ranges raw-ranges)]
    (reduce + (map (fn [[start end]] (+ (- end start) 1)) merged))))

(defn -main [& args]
  (->> args
       (first)
       (parse-file)
       (:ranges)
       (count-in-ranges)
       (println "> ")))
