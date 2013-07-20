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

(deftest test-get-state
  (testing "Accesses the current state of the parser."
    (is (= (get-state "foo")
           {:val "foo" :rest "foo"}))
    (is (= ((chain-parser parse-char (fn [_] get-state)) "foo")
           {:val "oo" :rest "oo"}))))

(deftest test-put-state
  (testing "Updates the current state of the parser."
    (is (= ((put-state "foo") "bar")
           {:val '() :rest "foo"}))
    (is (= ((chain-parser parse-char (fn [_] (put-state "foo"))) "bar")
           {:val '() :rest "foo"}))))

(deftest test-expect-char
  (testing "Succeeds on the given character and fails otherwise."
    (is (= ((expect-char \f) "foo")
           {:val \f :rest "oo"}))
    (is (nil? ((expect-char \f) "bar")))))

(deftest test-parse
  (testing "parse correctly chains together multiple parsers."
    (is (= ((parse
             (expect-char \[)
             [list (parse-while (partial not= \]))]
             (expect-char \])
             (constant list)) "[this is a list]")
           {:val "this is a list" :rest ""})))
  (testing "Returns nil on failure."
    (is (nil? ((parse
                (expect-char \[)
                [list (parse-while (partial not= \]))]
                (expect-char \]))
               "zazzle! pop!")))))

(deftest test-parse-or
  (testing "Returns the first parser's value if said parser parses."
    (is (= ((parse-or
             (expect-char \f)
             (parse-while (partial = \o)))
            "foo"))
        {:val \f :rest "oo"}))
  (testing "Returns the first non-nil parser value."
    (is (= ((parse-or
             (parse-while (partial = \f))
             (expect-char \z)
             (expect-char \f))
            "foo")
           {:val "f" :rest "oo"})))
  (testing "Returns nil if no parsers succeed."
    (is (nil? ((parse-or
                (expect-char \f)
                (expect-char \o)
                (parse-while (fn [c] (some #{c} "sdfoaij")))) "haha! this won't work")))))

(deftest test-parse-many
  (testing "Returns the correct parsed string."
    (is (= ((parse-many (expect-char \f)) "ffffoo")
           {:val "ffff" :rest "oo"})))
  (testing "Returns nil if it does not parse."
    (is (nil? ((parse-many
                (parse-while #(some #{%} "abcdefg")))
               "zazzle! pop!")))))

(deftest test-one-of
  (testing "Parses any of the supplied characters."
    (is (= ((one-of "abcd") "a string")
           {:val \a :rest " string"}))
    (is (= ((parse-many (one-of "abcdefg")) "gefbaabec!abacaba")
           {:val "gefbaabec" :rest "!abacaba"})))
  (testing "Returns nil if no characters are found."
    (is (nil? ((one-of "abcd") "ha, nope")))))

(deftest test-none-of
  (testing "Parses a single character if it is not to be excluded."
    (is (= ((none-of "abcd") "foo")
           {:val \f :rest "oo"})))
  (testing "Rejects any of the supplied characters."
    (is (nil? ((none-of "abc") "cfoo")))))

(deftest test-parse-string
  (testing "Parses the given string."
    (is (= ((parse-string "foo") "foobar")
           {:val "foo" :rest "bar"})))
  (testing "Returns nil if the string is not present."
    (is (nil? ((parse-string "foo") "fobar")))))
