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
  (testing "Correctly delimits by the given parsers, without returning them."
    (is (= ((parse-delimited (expect-char \b) (expect-char \:) (parse-many (not-char \:))) "barzoolie: ")
           {:val "arzoolie" :rest " "})))
  (testing "Correctly delimits by more complicated parsers."
    (is (= ((parse-delimited parse-int parse-float (parse-string-literal \')) "123'foo'456.789"))))
  (testing "Can parse the delimited empty string."
    (is (= ((parse-delimited (expect-char \|) (expect-char \|) (parse-many (not-char \|))) "||foo")
           {:val "" :rest "foo"})))
  (testing "Returns nil on failure."
    (is (= ((parse-delimited (expect-char \;) (expect-char \a) (parse-many (not-char \a))) "foo")))))

(deftest test-parse-escaped
  (testing "Correctly escapes characters."
    (is (= ((parse-escaped {\f \o}) "\\foo")
           {:val \o :rest "oo"})))
  (testing "Does not escape characters when not preceded by a backslash."
    (is (nil? ((parse-escaped {\f \o}) "foo"))))
  (testing "Doesn't affect characters not in the replacements map."
    (is (= ((parse-escaped {\f \o}) "\\pop")
           {:val \p :rest "op"}))))

(deftest test-parse-string-literal
  (testing "Correctly parses strings."
    (is (= ((parse-string-literal \") "\"foo\"bar")
           {:val "foo" :rest "bar"})))
  (testing "Correctly parses escaped characters."
    (is (= ((parse-string-literal \') "'\\'foo'bar")
           {:val "'foo" :rest "bar"})))
  (testing "Correctly parses unicode characters."
    (is (= ((parse-string-literal \') "'what\\u203d', they said")
           {:val "what‽" :rest ", they said"}))))

(def test-whitespace
  (testing "Parses whitespace."
    (is (= (whitespace " foo")
           {:val \space :rest "foo"}))))
