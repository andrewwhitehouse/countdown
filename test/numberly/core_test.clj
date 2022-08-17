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
    (is (= [[5] [2] [5 5] [5 2] [5 5 2]] (candidates [5 2 5]))))
  (testing "three distinct elements"
    (is (= [[1] [2] [3] [1 2] [1 3] [2 3] [1 2 3]] (candidates [1 2 3])))))

(deftest test-apply-operator
  (testing "first case"
    (is (= [{:numbers [3 3] :steps [{:left 1 :right 2 :op '+ :result 3}]}
            {:numbers [1 5] :steps [{:left 2 :right 3 :op '+ :result 5}]}]
           (apply-operator {:numbers [1 2 3] :steps []} '+)))))

(deftest test-solve
  (testing "first case"
    (let [expected-result [{:numbers (7), :steps [{:left 3, :right 4, :op +, :result 7}]}
                           {:numbers (7), :steps [{:left 1, :right 2, :op +, :result 3}
                                                  {:left 3, :right 4, :op +, :result 7}]}
                           {:numbers (7), :steps [{:left 2, :right 4, :op +, :result 6}
                                                  {:left 1, :right 6, :op +, :result 7}]}]]
      (is (= expected-result (solve [1 2 3 4] 7 '+))))))
