(ns numberly.core-test
  (:require [clojure.test :refer :all]
            [numberly.core :refer :all]))

(comment deftest select-numbers
  (testing "number selection"
    (let [selected (numbers 4 2)
          _ (println selected)]
    (is (= 6 (count selected))))))

(comment deftest test-sums
  (testing "finds addends"
    (is (= '([1 1 2 5 10]) (sums [1 1 2 5 10 20] 19)))
    (is (= '([1 5 10] [1 5 10]) (sums [1 1 2 5 10 20] 16)))
    (is (= '() (sums [1 1 2 5 10 20] 40)))))

(deftest test-candidates
  (testing "single element"
    (is (= [[25]] (candidates [25]))))
  (testing "two distinct elements"
    (is (= [[1] [3] [1 3]] (candidates [1 3]))))
  (testing "duplicate elements"
    (is (= [[5] [2] [2 5] [2 5 5]] (candidates [5 2 5])))))
