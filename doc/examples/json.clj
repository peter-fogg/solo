(ns solo.examples.json
  (:require [solo.core :refer :all]
            [solo.utils :refer :all]))

;; A simple JSON parser, as an example of solo usage.

(declare parse-json)

(def test-object "{\"foo\" : 3 , \"bar\": {\"pies\": [3.4,   2 ]} }")

(def spaces
  "Maybe parse lots of whitespace."
  (parse-maybe (parse-many whitespace)))

(defn wrap-spaces
  "Parse parse, possibly surrounded by whitespace."
  [parser]
  (parse
   spaces
   [string parser]
   spaces
   (constant string)))

(def comma
  "The separator for objects and arrays."
  (wrap-spaces (expect-char \,)))

(def json-object-field
  "Parse a single field (key: value) of a JSON object. We'll represent
   keys (usually strings) as Clojure keywords."
  (parse
   [key (parse-string-literal \")]
   (wrap-spaces (expect-char \:))
   [value parse-json]
   (constant [(keyword key) value])))

(def json-object
  "Parse the whole JSON object."
  (map-parser
   #(into {} %)
   (parse-delimited
    \{ \}
    (wrap-spaces (sep-by json-object-field comma)))))

(def json-array
  "Parse a JSON array."
  (parse-delimited
   \[ \]
   (wrap-spaces (sep-by parse-json comma))))

(def json-number
  (parse-or parse-float
            parse-int))

(def parse-json
  "Parse that JSON."
  (parse
   (parse-or
    json-object
    json-array
    json-number
    (parse-string-literal \"))))

(print (parse-json test-object))
