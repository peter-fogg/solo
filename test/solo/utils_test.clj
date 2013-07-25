(ns solo.utils-test
  (:require [clojure.test :refer :all]
            [solo.utils :refer :all]))

(deftest test-parse-int
  (testing "Parses an integer correctly."
    (is (= (parse-int "123foo")
           {:val 123 :rest "foo"})))
  (testing "Returns nil on failure."
    (is (nil? (parse-int "foo")))))

