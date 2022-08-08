(ns numberly.core-test
  (:require [clojure.test :refer :all]
            [numberly.core :refer :all]))

(deftest select-numbers
  (testing "number selection"
    (let [selected (numbers 4 2)
          _ (println selected)]
    (is (= 6 (count selected))))))
