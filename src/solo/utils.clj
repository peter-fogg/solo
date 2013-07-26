(ns solo.utils
  (:require [solo.core :refer :all]))

(def parse-int
  "Parse an integer."
  (parse
   [sign (parse-maybe (expect-char \-))]
   [num (parse-while #(some #{%} "0123456789"))]
   ;; We'll be lazy and delegate to Clojure's read-string for this
   (constant (* (read-string num) (if sign -1 1)))))

(def parse-float
  "Parse a floating-point number."
  (parse
   [sign (parse-maybe (expect-char \-))]
   [integer parse-int]
   (expect-char \.)
   [fractional (parse-while #(some #{%} "0123456789"))]
   ;; This is awful and I feel bad about it
   (constant (* (read-string (str integer \. fractional))
                (if sign -1 1)))))
   ;; In case you were wondering

(defn parse-delimited
 "Parse `parser` delimited by the given `begin` and `end`
 characters."
 [begin end parser]
 (parse
  (expect-char begin)
  [string parser]
  (expect-char end)
  (constant string)))

(def parse-string-literal
  "Parse a string literal, delimited by double quotes."
  (parse-delimited \" \" (parse-many (not-char \"))))

(def parse-string-literal-option
  "Allow the option of single or double quotes in a string literal."
  (parse-or (parse-delimited \' \' (parse-many (not-char \')))
            parse-string-literal))

(def whitespace
  (one-of " \n\t\r"))
