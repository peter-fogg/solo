(ns solo.utils-test
  (:require [clojure.test :refer :all]
            [solo.core :refer :all]
            [solo.utils :refer :all]))

(deftest test-parse-int
  (testing "Parses an integer correctly."
    (is (= (parse-int "123foo")
           {:val 123 :rest "foo"})))
  (testing "Parses negative integers."
    (is (= (parse-int "-1234foo")
           {:val -1234 :rest "foo"})))
  (testing "Returns nil on failure."
    (is (nil? (parse-int "foo")))))

(deftest test-parse-float
  (testing "Parses a float correctly."
    (is (= (parse-float "123.03foo")
           {:val 123.03 :rest "foo"})))
  (testing "Parses negative floats."
    (is (= (parse-float "-123.03foo")
           {:val -123.03 :rest "foo"})))
  (testing "Returns nil on failure."
    (is (nil? (parse-int "foo")))))

(deftest test-parse-delimited
  (testing "Correctly delimits by the given characters, without returning them."
    (is (= ((parse-delimited \b \: (parse-many (not-char \:))) "barzoolie: ")
           {:val "arzoolie" :rest " "})))
  (testing "Can parse the delimited empty string."
    (is (= ((parse-delimited \| \| (parse-many (not-char \|))) "||foo")
           {:val "" :rest "foo"})))
  (testing "Returns nil on failure."
    (is (= ((parse-delimited \; \a (parse-many (not-char \a))) "foo")))))

(def test-whitespace
  (testing "Parses whitespace."
    (is (= (whitespace " foo")
           {:val \space :rest "foo"}))))
