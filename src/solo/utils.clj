(ns solo.utils
  (:require [solo.core :refer :all]))

(def parse-int
  "Parse an integer."
  (parse
   [sign (parse-maybe (expect-char \-))]
   [num (parse-while #(some #{%} "0123456789"))]
   (constant (* (read-string num) (if sign -1 1)))))

(defn parse-delimited
 "Parse a string delimited by the given `begin` and `end`
 characters."
 [begin end]
 (parse
  (expect-char begin)
  [string (parse-many (none-of (list begin end)))]
  (expect-char end)
  (constant string)))

(def parse-string-literal
  "Parse a string literal, delimited by double quotes."
  (parse-delimited \" \"))

(def parse-string-literal-option
  "Allow the option of single or double quotes in a string literal."
  (parse-or (parse-delimited \' \')
            parse-string-literal))
