(ns solo.utils-test
  (:require [clojure.test :refer :all]
            [solo.core :refer :all]
            [solo.utils :refer :all]))

(deftest test-parse-int
  (testing "Parses an integer correctly."
    (is (= (parse-int (to-parse "123foo"))
           {:val 123 :rest "foo" :pos 3})))
  (testing "Parses negative integers."
    (is (= (parse-int (to-parse "-1234foo"))
           {:val -1234 :rest "foo" :pos 5})))
  (testing "Returns an error on failure."
    (is (= (parse-int (to-parse "foo"))
           {:err "parse-while" :pos 0}))))

(deftest test-parse-float
  (testing "Parses a float correctly."
    (is (= (parse-float (to-parse "123.03foo"))
           {:val 123.03 :rest "foo" :pos 6})))
  (testing "Parses negative floats."
    (is (= (parse-float (to-parse "-123.03foo"))
           {:val -123.03 :rest "foo" :pos 7})))
  (testing "Returns an error on failure."
    (is (= (parse-int (to-parse "foo"))
           {:err "parse-while" :pos 0}))))

(deftest test-parse-delimited
  (testing "Correctly delimits by the given parsers, without returning them."
    (is (= ((parse-delimited (expect-char \b) (expect-char \:) (parse-many (not-char \:))) (to-parse "barzoolie: "))
           {:val '(\a \r \z \o \o \l \i \e) :rest " " :pos 10})))
  (testing "Correctly delimits by more complicated parsers."
    (is (= ((parse-delimited parse-int parse-float (parse-string-literal \')) (to-parse "123'foo'456.789"))
           {:val "foo" :rest "" :pos 15})))
  (testing "Can parse the delimited empty string."
    (is (= ((parse-delimited (expect-char \|) (expect-char \|) (parse-many (not-char \|))) (to-parse "||foo"))
           {:val [] :rest "foo" :pos 2})))
  (testing "Returns an error on failure."
    (is (= ((parse-delimited (expect-char \;) (expect-char \a) (parse-many (not-char \a))) (to-parse "foo"))
           {:err "character ';'" :pos 0}))))

(deftest test-parse-escaped
  (testing "Correctly escapes characters."
    (is (= ((parse-escaped {\f \o}) (to-parse "\\foo"))
           {:val \o :rest "oo" :pos 2})))
  (testing "Does not escape characters when not preceded by a backslash."
    (is (= ((parse-escaped {\f \o}) (to-parse "foo"))
           {:err "character '\\'" :pos 0})))
  (testing "Doesn't affect characters not in the replacements map."
    (is (= ((parse-escaped {\f \o}) (to-parse "\\pop"))
           {:val \p :rest "op" :pos 2}))))

(deftest test-parse-string-literal
  (testing "Correctly parses strings."
    (is (= ((parse-string-literal \") (to-parse "\"foo\"bar"))
           {:val "foo" :rest "bar" :pos 5})))
  (testing "Correctly parses escaped characters."
    (is (= ((parse-string-literal \') (to-parse "'\\'foo'bar"))
           {:val "'foo" :rest "bar" :pos 7})))
  (testing "Correctly parses unicode characters."
    (is (= ((parse-string-literal \') (to-parse "'what\\u203d', they said"))
           {:val "what‽" :rest ", they said" :pos 12}))))

(def test-whitespace
  (testing "Parses whitespace."
    (is (= (whitespace (to-parse " foo"))
           {:val \space :rest "foo" :pos 1}))))
