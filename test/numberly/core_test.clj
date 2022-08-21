(ns numberly.core-test
  (:require [clojure.test :refer :all]
            [numberly.core :refer :all]
            [clojure.pprint :refer [pprint]]))

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
    (is (= [{:numbers [3 3] :steps [{:left 2 :right 1 :op '+ :result 3}]}
            {:numbers [4 2] :steps [{:left 3 :right 1 :op '+ :result 4}]}
            {:numbers [5 1] :steps [{:left 3 :right 2 :op '+ :result 5}]}]
           (apply-operator {:numbers [1 2 3] :steps []} '+ 10)))))

(deftest test-solve
  (comment testing "first case"
           (let [expected-result [{:numbers '(7), :steps [{:left 4, :right 3, :op '+, :result 7}]}
                                  {:numbers '(7), :steps [{:left 2, :right 1, :op '+, :result 3}
                                                          {:left 4, :right 3, :op '+, :result 7}]}
                                  {:numbers '(7), :steps [{:left 4, :right 2, :op '+, :result 6}
                                                          {:left 6, :right 1, :op '+, :result 7}]}]]
             (is (= expected-result (solve [1 2 3 4] 7 ['+])))))
  (comment testing "addition and multiplication"
           (let [expected-result [{:numbers [7], :steps [{:left 4, :right 3, :op '+, :result 7}]}
                                  {:numbers [7],
                                   :steps
                                            [{:left 3, :right 2, :op '*, :result 6}
                                             {:left 6, :right 1, :op '+, :result 7}]}
                                  {:numbers [7],
                                   :steps
                                            [{:left 2, :right 1, :op '+, :result 3}
                                             {:left 4, :right 3, :op '+, :result 7}]}
                                  {:numbers [7],
                                   :steps
                                            [{:left 4, :right 2, :op '+, :result 6}
                                             {:left 6, :right 1, :op '+, :result 7}]}
                                  {:numbers [7],
                                   :steps
                                            [{:left 4, :right 3, :op '+, :result 7}
                                             {:left 7, :right 1, :op '*, :result 7}]}
                                  {:numbers [7],
                                   :steps
                                            [{:left 3, :right 1, :op '*, :result 3}
                                             {:left 4, :right 3, :op '+, :result 7}]}]
                 actual-result (solve [1 2 3 4] 7 ['+ '*])
                 _ (pprint actual-result)]
             (is (= expected-result (solve [1 2 3 4] 7 ['+ '*])))))
  (comment testing "addition, multiplication and subtraction"
           (let [expected-result [{:numbers [15],
                                   :steps
                                            [{:left 7, :right 4, :op '-, :result 3}
                                             {:left 5, :right 3, :op '*, :result 15}]}]
                 actual-result (solve [7 4 5] 15 ['+ '* '-])]
             (is (= expected-result actual-result))))
  (comment testing "division"
           (let [expected-result [{:numbers [3], :steps [{:left 75, :right 25, :op '/, :result 3}]}]
                 actual-result (solve [75 25] 3 ['/])]
             (is (= expected-result actual-result))))
  (comment testing "division and subtraction"
           (let [expected-result [{:numbers [10], :steps [{:left 75, :right 25, :op '-, :result 50}
                                                          {:left 50, :right 5, :op '/, :result 10}]}]
                 actual-result (solve [75 25 5] 10 ['/ '-])]
             (is (= expected-result actual-result))))
  (testing "full test"
    (let [expected-result [{:numbers [843]
                            :steps   [{:left 100 :right 3 :op '+ :result 103}
                                      {:left 103 :right 1 :op '- :result 102}
                                      {:left 102 :right 9 :op '* :result 918}
                                      {:left 918 :right 75 :op '- :result 843}]}
                           {:numbers [843]
                            :steps   [{:left 3 :right 1 :op '- :result 2}
                                      {:left 100 :right 2 :op '+ :result 102}
                                      {:left 102 :right 9 :op '* :result 918}
                                      {:left 918 :right 75 :op '- :result 843}]}
                           {:numbers [843]
                            :steps   [{:left 100 :right 1 :op '- :result 99}
                                      {:left 99 :right 3 :op '+ :result 102}
                                      {:left 102 :right 9 :op '* :result 918}
                                      {:left 918 :right 75 :op '- :result 843}]}]
          actual-result (solve [1 9 3 75 100] 843 ['+ '- '* '/])]
      (is (= expected-result actual-result)))))

(deftest test-pair-indexes
  (testing "pair indexes"
    (is (= [[0 1] [0 2] [0 3] [1 2] [1 3] [2 3]] (pairs-indexes [1 2 3 4])))))

(deftest test-remove-two
  (testing "remove first"
    (is (= [2] (remove-two [0 1 2] 0 1))))
  (testing "second"
    (is (= [0] (remove-two [0 1 2] 1 2))))
  (testing "last"
    (is (= [1] (remove-two [0 1 2] 0 2))))
  (testing "remove two"
    (is (= [] (remove-two [0 1] 0 1)))))