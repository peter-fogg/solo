(ns solo.core-test
  (:require [clojure.test :refer :all]
            [solo.core :refer :all]))

(deftest test-parse-char
  (testing "`parse-char` should return the first character and the rest of the string."
    (is (= (parse-char (to-parse "foo"))
           {:val \f :rest "oo" :pos 1})))
  (testing "Should return an error on the empty string."
    (is (= (parse-char (to-parse ""))
           {:err "any character" :pos 0}))))

(deftest test-constant
  (testing "The constant parser always gives back its value."
    (is (= ((constant "foo") (to-parse "bar"))
           {:val "foo" :rest "bar" :pos 0}))))

(deftest test-map-parser
  (testing "`map-parser` applies a function to the result of a parser."
    (is (= ((map-parser int parse-char) (to-parse "foo"))
           {:val 102 :rest "oo" :pos 1})))
  (testing "Returns an error on failure."
    (is (= ((map-parser int parse-char) (to-parse ""))
           {:err "any character" :pos 0}))))

(deftest test-parse-while
  (testing "`parse-while` consumes as long as its predicate is true."
    (is (= ((parse-while #(= % \f)) (to-parse "fffooo"))
           {:val "fff" :rest "ooo" :pos 3})))
  (testing "Returns an error on failure."
    (is (= ((parse-while #(= % \f)) (to-parse "ooooo"))
           {:err "parse-while" :pos 0}))))

(deftest test-chain-parser
  (testing "Correctly chains two parsers."
    (is (= ((chain-parser (expect-char \f) (fn [_] (expect-char \o))) (to-parse "foo"))
           {:val \o :rest "o" :pos 2})))
  (testing "Returns an error if either parser fails."
    (is (and (= ((chain-parser (expect-char \f) (fn [_] (expect-char \f))) (to-parse "foo"))
                {:err "character 'f'" :pos 1})
             (= ((chain-parser (expect-char \o) (fn [_] (expect-char \o))) (to-parse "foo"))
                {:err "character 'o'" :pos 0})))))

(deftest test-get-state
  (testing "Accesses the current state of the parser."
    (is (= (get-state (to-parse "foo"))
           {:val "foo" :rest "foo" :pos 0}))
    (is (= ((chain-parser parse-char (fn [_] get-state)) (to-parse "foo"))
           {:val "oo" :rest "oo" :pos 1}))))

(deftest test-put-state
  (testing "Updates the current state of the parser."
    (is (= ((put-state "foo") (to-parse "bar"))
           {:val '() :rest "foo" :pos 0}))
    (is (= ((chain-parser parse-char (fn [_] (put-state "foo"))) (to-parse "bar"))
           {:val '() :rest "foo" :pos 1}))))

(deftest test-expect-char
  (testing "Succeeds on the given character and fails otherwise."
    (is (= ((expect-char \f) (to-parse "foo"))
           {:val \f :rest "oo" :pos 1}))
    (is (= ((expect-char \f) (to-parse "bar"))
           {:err "character 'f'" :pos 0}))))

(deftest test-parse
  (testing "parse correctly chains together multiple parsers."
    (is (= ((parse
             (expect-char \[)
             [list (parse-while (partial not= \]))]
             (expect-char \])
             (constant list)) (to-parse "[this is a list]"))
           {:val "this is a list" :rest "" :pos (count "[this is a list]")})))
  (testing "Returns an error on failure."
    (is (= ((parse
             (expect-char \[)
             [list (parse-while (partial not= \]))]
             (expect-char \]))
            (to-parse "zazzle! pop!"))
           {:err "character '['" :pos 0}))))

(deftest test-parse-or
  (testing "Returns the first parser's value if said parser parses."
    (is (= ((parse-or
             (expect-char \f)
             (parse-while (partial = \o)))
            (to-parse "foo")))
        {:val \f :rest "oo" :pos 1}))
  (testing "Returns the first non-nil parser value."
    (is (= ((parse-or
             (parse-while (partial = \f))
             (expect-char \z)
             (expect-char \f))
            (to-parse "foo"))
           {:val "f" :rest "oo" :pos 1})))
  (testing "Returns an error if no parsers succeed."
    (is (= ((parse-or
             (expect-char \f)
             (expect-char \o)
             (parse-while (fn [c] (some #{c} "sdfoaij")))) (to-parse "haha! this won't work"))
           {:err "character 'f'" :pos 0}))))

(deftest test-parse-many
  (testing "Returns the correct parsed string."
    (is (= ((parse-many (expect-char \f)) (to-parse "ffffoo"))
           {:val '(\f \f \f \f) :rest "oo" :pos 4})))
  (testing "Returns the empty string parser if it does not parse."
    (is (= ((parse-many (expect-char \f)) (to-parse "nope"))
           {:val [] :rest "nope" :pos 0}))))

(deftest test-parse-many1
  (testing "Returns the correct parsed string."
    (is (= ((parse-many-1 (expect-char \f)) (to-parse "ffffoo"))
           {:val '(\f \f \f \f) :rest "oo" :pos 4})))
  (testing "Returns an error if it does not parse."
    (is (= ((parse-many-1
             (parse-while #(some #{%} "abcdefg")))
            (to-parse "zazzle! pop!"))
           {:err "parse-while" :pos 0}))))

(deftest test-one-of
  (testing "Parses any of the supplied characters."
    (is (= ((one-of "abcd") (to-parse "a string"))
           {:val \a :rest " string" :pos 1}))
    (is (= ((parse-many (one-of "abcdefg")) (to-parse "gefbaabec!abacaba"))
           {:val '(\g \e \f \b \a \a \b \e \c) :rest "!abacaba" :pos 9})))
  (testing "Returns an error if no characters are found."
    (is (= ((one-of "abcd") (to-parse "ha, nope"))
           {:err "satisfy" :pos 0}))))

(deftest test-none-of
  (testing "Parses a single character if it is not to be excluded."
    (is (= ((none-of "abcd") (to-parse "foo"))
           {:val \f :rest "oo" :pos 1})))
  (testing "Rejects any of the supplied characters."
    (is (= ((none-of "abc") (to-parse "cfoo"))
           {:err "satisfy" :pos 0}))))

(deftest test-parse-string
  (testing "Parses the given string."
    (is (= ((parse-string "foo") (to-parse "foobar"))
           {:val "foo" :rest "bar" :pos 3})))
  (testing "Returns an error if the string is not present."
    (is (= ((parse-string "foo") (to-parse "fobar"))
           {:err "character 'o'" :pos 2}))))

(deftest test-sep-by1
  (testing "Separates one parser's output by another."
    (is (= ((sep-by-1
             (parse-many (one-of "0123456789"))
             (parse-while #(Character/isWhitespace %)))
            (to-parse "123 456 789"))
           {:val '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9)) :rest "" :pos 11})))
  (testing "Correctly leaves the rest of the string."
    (is (= ((sep-by-1
             (parse-many (one-of "0123456789"))
             (parse-while #(Character/isWhitespace %)))
            (to-parse "123 456 789foo "))
           {:val '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9)) :rest "foo " :pos 11})))
  (testing "Returns an error if the first parser does not parse."
    (is (= ((sep-by-1
             (expect-char \f)
             (parse-many (expect-char \o)))
            (to-parse "ofoo"))
           {:err "character 'f'" :pos 0}))))

(deftest test-sep-by
  (testing "Acts like sep-by-1."
    (is (= ((sep-by
             (parse-many (one-of "0123456789"))
             (parse-while #(Character/isWhitespace %)))
            (to-parse "123 456 789"))
           {:val '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9)) :rest "" :pos 11})))
  (testing "Returns the empty list if the first parser does not parse."
    (is (= ((sep-by
             (expect-char \f)
             (parse-many (expect-char \o)))
            (to-parse "ofoo"))
           {:val [] :rest "ofoo" :pos 0}))))

(deftest test-parse-maybe
  (testing "Returns the correct value on a successful parse."
    (is (= ((parse-maybe (parse-string "foobar")) (to-parse "foobarbaz"))
           {:val "foobar" :rest "baz" :pos 6})))
  (testing "Returns a constant nil value if parser fails."
    (is (= ((parse-maybe (parse-string "zazzle")) (to-parse "foobarbaz"))
           {:val nil :rest "foobarbaz" :pos 0}))))

(deftest test-parse-n
  (testing "Parses exactly n occurrences of parser."
    (is (= ((parse-n 4 (expect-char \f)) (to-parse "ffffffooo"))
           {:val '(\f \f \f \f) :rest "ffooo" :pos 4})))
  (testing "Returns an error if less than n are present."
    (is (= ((parse-n 4 (expect-char \f)) (to-parse "foo"))
           {:err "character 'f'" :pos 1}))))
