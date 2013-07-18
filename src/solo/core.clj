(ns solo.core)

;; We'll represent a parse state as a map of {:val value, :rest
;; string}. A parser is then a function of (string -> parse
;; state). Those of you familiar with Haskell may recognize this as a
;; monad. It is!

;; Our first parser. How exciting!
(defn parse-char
  "Parse a single character."
  [state]
  (if (empty? state)
    {:val ""
     :rest ""}
    {:val (first state)
     :rest (rest state)}))

;; The constant parser simply returns a parser that always gives the
;; same value. Does this sound familiar?
(defn constant
  "The constant parser."
  [val]
  (fn [state]
    {:val val
     :rest state}))

;; It's often useful to apply some function to the result of a
;; parser. Let's do that:
(defn map-parser
  "Map `f` across the result of `parser`."
  [f parser]
  (fn [state]
    (let [result (parser state)]
      (assoc result :val (f (:val result))))))

;; Let's see an example of using map-parser. First, we'll define
;; another useful function, parse-while.
(defn parse-while
  "Return a parser consuming as long as `pred` is true."
  [pred]
  (fn [state]
    {:val (apply str (take-while pred state))
     :rest (apply str (drop-while pred state))}))

;; Now we'll use parse-while for a quite useful purpose: parsing
;; numbers. For now we'll only parse positive integers; we'll fix that
;; later.

;; Now, the real deal.
(defn parse-int
  "Parse an integer."
  [state]
  ((map-parser read-string
               (parse-while
                #(some #{%} "0123456789")))
   state))
