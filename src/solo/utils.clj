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

(defn parse-escaped
  [chars]
  (parse
   (expect-char \\)
   [char parse-char]
   (constant (or (chars char) char))))

(def escape-chars
  "Normal escape characters for string literals."
  {\n \newline
   \r \return
   \t \tab
   \b \backspace
   \f \formfeed
   \" \"
   \' \'
   \\ \\})

(def parse-unicode
  (parse
   (expect-char \\)
   (expect-char \u)
   (map-parser
    #(char (Integer/parseInt (apply str %) 16))
    (parse-n 4 (one-of "0123456789abcdef")))))

(defn parse-string-literal
  "Parse a string literal, delimited by `quote`, with escaping."
  [quote]
  (parse-delimited
   quote quote
   (parse-many
    (parse-or
     parse-unicode
     (parse-escaped escape-chars)
     (not-char quote)))))

(def whitespace
  (one-of " \n\t\r"))
