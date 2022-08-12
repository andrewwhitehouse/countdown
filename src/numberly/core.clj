(ns numberly.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn numbers [small-count large-count]
  (if (not= 6 (+ small-count large-count))
    (throw (IllegalArgumentException. "count of numbers needed must be 6")))
  (if (> large-count 4)
    (throw (IllegalArgumentException. "maximum of 4 large numbers allowed")))
  (let [large (take large-count (shuffle [25 50 75 100]))
        small (take small-count (shuffle (repeatedly 20 #(inc (rand-int 10)))))]
    (concat small large)))

(defn sums
  ([numbers total] (sums numbers total {:matched [] :candidates [[]]}))
  ([numbers total working]
   (if-let [number (first numbers)]
     (let [new-candidates (->> working
                               :candidates
                               (mapv #(conj % number))
                               (group-by #(= total (apply + %))))]
       (recur
         (rest numbers)
         total
         (-> working
             (update :matched concat (new-candidates true))
             (update :candidates concat (new-candidates false)))))
     (working :matched))))

(defn candidates [numbers]
  (let [cnt (count numbers)]
    (distinct
      (map
        (fn [{:keys [len start-index]}]
          (->> numbers (drop start-index) (take len) (sort)))
        (for [len (range 1 (inc cnt))
              start-index (range cnt)
              :when (<= (+ len start-index) cnt)]
          {:len len :start-index start-index})))))

(defn -main [& args]
  (println "Numbers" (str/join " " (numbers 4 2)))
  (println "Target" (+ 200 (rand-int 800))))

(comment

  (require '[clojure.math.combinatorics :as combo])
  (mapcat #(combo/combinations [1, 9, 3, 1, 75, 100] %) (range 2 7))

  )

(comment
  (def cnt 3)
  (for [len (range 1 (inc cnt))
        start-index (range cnt)]
    {:len len :start-index start-index})
  )


(comment

  (def numbers [1 9 3 1 75 100])
  (def combos (mapcat #(combo/combinations numbers %) (range 2 7)))

  )