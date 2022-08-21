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

(defn candidates [numbers]
  (mapcat
    (fn [cnt]
      (combo/combinations numbers cnt))
    (range 1 (inc (count numbers)))))

(defn candidate-steps [candidates]
  (map (fn [numbers] {:numbers numbers :steps []}) candidates))

(defn replace-pair [coll index new-value]
  (if (> index (- (count coll) 2))
    (throw (IllegalArgumentException. "index out of range")))
  (concat
    (take index coll)
    [new-value]
    (take (- (count coll) 2) (drop (+ 2 index) coll))))

(defn apply-to-pair [numbers operator index1 index2]
  (let [a (nth numbers index1)
        b (nth numbers index2)
        larger (Math/max a b)
        smaller (Math/min a b )
        result (condp = operator
                 '+ (+ a b)
                 '* (* a b)
                 '- (when (not= larger smaller) (- larger smaller))
                 '/ (if (zero? (rem larger smaller)) (/ larger smaller) nil)
                 nil)
        _ (comment println "larger" larger "smaller" smaller "result" result)]
    (when result
      {:left larger :right smaller :op operator :result result})))

(defn pairs-indexes [coll]
  (let [cnt (count coll)]
    (for [a (range cnt)
          b (range (inc a) cnt)]
      [a b])))

(defn remove-two
  "remove elem in coll"
  [coll index1 index2]
  (->> (map (fn [idx item] [idx item]) (iterate inc 0) coll)
       (remove (fn [[idx item]] (or (= index1 idx) (= index2 idx))))
       (map second)))

(defn debug-> [x message]
  (println message x)
  x)

(defn apply-operator [{:keys [numbers steps]} operator total]
  (keep (fn [[index1 index2]]
               (if-let [{:keys [left right result] :as m}
                        (apply-to-pair numbers operator index1 index2)]
                 {:numbers (-> numbers
                                 (remove-two index1 index2)
                                 (conj result))
                    :steps (conj steps {:left left :right right :op operator :result result})}))
          (pairs-indexes numbers)))

(defn add-all [vec coll]
  (reduce
    (fn [acc item] (conj acc item))
    vec
    coll))

(defn iterate-operator [{:keys [numbers steps] :as candidate-steps} operators total]
  (loop [remaining [candidate-steps]
         matched []]
    (if-let [candidate (first remaining)]
      (let [results (mapcat #(apply-operator candidate % total) operators)
            matched? (fn [candidate] (and (= 1 (count (:numbers candidate)))
                                       (= total (first (:numbers candidate)))))
            new-matches (filter matched? results)
            unmatched (remove matched? results)]
        (recur
          (concat
            (rest remaining)
            (filter #(> (count (:numbers %)) 1) unmatched))
          (concat matched new-matches)))
      matched)))



(defn debug->> [message x]
  (println message x)
  x)

(defn solve [numbers total operators]
  (->> numbers
      candidates
     (debug->> "candidates")
      candidate-steps
      (mapcat #(iterate-operator % operators total))
       distinct))

(defn -main [& args]
  (println "Numbers" (str/join " " (numbers 4 2)))
  (println "Target" (+ 200 (rand-int 800))))

(defn -main [& args]
  (let [before (System/currentTimeMillis)
        result (solve [1 9 3 75 100] 843 ['+ '- '* '/])
        after (System/currentTimeMillis)]
    (println "result" result)
    (println "time taken" (- after before) "ms")))