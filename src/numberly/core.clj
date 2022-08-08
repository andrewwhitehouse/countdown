(ns numberly.core
  (:require [clojure.string :as str]))

(defn numbers [small-count large-count]
  (if (not= 6 (+ small-count large-count))
    (throw (IllegalArgumentException. "count of numbers needed must be 6")))
  (if (> large-count 4)
    (throw (IllegalArgumentException. "maximum of 4 large numbers allowed")))
  (let [large (take large-count (shuffle [25 50 75 100]))
        small (take small-count (shuffle (repeatedly 20 #(inc (rand-int 10)))))]
    (concat small large)))

(defn -main [& args]
  (println "Numbers" (str/join " " (numbers 4 2)))
  (println "Target" (+ 200 (rand-int 800))))