(ns solo.core-test
  (:require [clojure.test :refer :all]
            [solo.core :refer :all]))

(deftest test-parse-char
  (testing "`parse-char` should return the first character and the rest of the string."
    (is (= {:val \f :rest "oo"}
           (parse-char "foo"))))
  (testing "Should return `nil` on the empty string."
    (is (nil? (parse-char "")))))

(deftest test-constant
  (testing "The constant parser always gives back its value."
    (is (= ((constant "foo") "bar")
           {:val "foo" :rest "bar"}))))

(deftest test-map-parser
  (testing "`map-parser` applies a function to the result of a parser."
    (is (= ((map-parser int parse-char) "foo")
           {:val 102 :rest "oo"})))
  (testing "Returns nil on failure."
    (is (nil? ((map-parser int parse-char) "")))))

(deftest test-parse-while
  (testing "`parse-while` consumes as long as its predicate is true."
    (is (= ((parse-while #(= % \f)) "fffooo")
           {:val "fff" :rest "ooo"})))
  (testing "Returns nil on failure."
    (is (nil? ((parse-while #(= % \f)) "ooooo")))))

(deftest test-parse-int
  (testing "Parses an integer correctly."
    (is (= (parse-int "123foo")
           {:val 123 :rest "foo"})))
  (testing "Returns nil on failure."
    (is (nil? (parse-int "foo")))))
