(ns solo.core)

;; We'll represent a parse state as a map of {:val value, :rest
;; string}. A parser is then a function of (string -> parse
;; state). Those of you familiar with Haskell may recognize this as a
;; monad. It is!

;; An example of a parser. How exciting!
(comment
  (defn parse-char
    "Parse a single character."
    [state]
    (if (empty? state)
      nil
      {:val (first state)
       :rest (apply str (rest state))})))

;; We can do better, though -- our above implementation of parse-char
;; isn't very general. Let's abstract a bit.
(defn satisfy
  "Returns a parser accepting a character satisfying `pred`."
  [pred]
  (fn [state]
    (let [c (first state)]
      (when (and c (pred c))
        {:val c
         :rest (apply str (rest state))}))))

;; Let's use satisfy to implement parse-char.
(def parse-char (satisfy (fn [_] true)))

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
    (when-let [result (parser state)]
      (assoc result :val (f (:val result))))))

;; Let's see an example of using map-parser. First, we'll define
;; another useful function, parse-while.
(defn parse-while
  "Return a parser consuming as long as `pred` is true."
  [pred]
  (fn [state]
    (let [val (apply str (take-while pred state))
          rest (apply str (drop-while pred state))]
      (if (empty? val)
        nil
        {:val val
         :rest rest}))))

;; Now we'll use parse-while for a quite useful purpose: parsing
;; numbers. For now we'll only parse positive integers; we'll fix that
;; later.
(def parse-int
  (map-parser read-string (parse-while #(some #{%} "0123456789"))))

;; We won't always be parsing integers. It's good to have some way to
;; run one parser, then the next (and possibly the next!). Chaining
;; parsers allows us to do this, making the value of one parser
;; available to the next and updating the state correctly.
(defn chain-parser
  "Combine two parsers into a new one."
  [left right]
  (fn [state]
    (when-let [{val :val new-state :rest} (left state)]
      ((right val) new-state))))

;; We can use the state of our parser with chain-parser, but these two
;; functions allow us to directly access and modify it.
(defn get-state
  "Get the current state of the parser."
  [state]
  {:val state :rest state})

(defn put-state
  "Modify the state of the parser."
  [state]
  (fn [_]
    {:val '() :rest state}))

;; Using chain-parser can get tedious and error-prone, what with all
;; the nested functions. We define a macro, parse, which cleans up the
;; process for us.
(defmacro parse [& body]
  (cond
   (nil? body) '()
   (= 1 (count body)) (first body)
   :else (let [f (first body)
               [parser param] (if (vector? f)
                                [(second f) [(first f)]]
                                [f `[_#]])]
           (list 'chain-parser
                 parser
                 `(fn ~param
                    ~`(parse ~@(rest body)))))))

;; This function allows us to expect a single character, and fail if
;; it isn't in our string to parse.
(defn expect-char
  "Expect `char`, and fail if not present."
  [char]
  (satisfy (partial = char)))

;; What if we have a choice of values to parse? parse-or lets us try
;; one parser, then the next, then the next, and so on.
(defn parse-or
  "Choice of several parsers."
  [& parsers]
  (fn [state]
    (some #(% state) parsers)))

;; The parse-many combinator allows us to collect any number of a certain
;; parser, and return a list of what we find.
(defn parse-many
  "Repeatedly apply `parser`."
  [parser]
  (fn [state]
    ((parse
      [val parser]
      [rest (parse-or (parse-many parser) (constant []))]
      (constant (apply str (cons val rest))))
     state)))

;; We can use one-of to give us a choice between any of a number of
;; characters.
(defn one-of
  "Parse any of the supplied characters."
  [chars]
  (satisfy #(some (partial = %) chars)))

;; If we want to assert that a character is not present in our string,
;; we can use not-char. It is the opposite of expect-char.
(defn not-char
  "Reject the supplied character."
  [char]
  (fn [state]
    (when-not (= char (first state))
      {:val (str (first state))
       :rest (apply str (rest state))})))

;; none-of is the just opposite of one-of -- it ensures that it does
;; not see any of the given characters.
(defn none-of
  "Reject any of the supplied characters."
  [chars]
  (satisfy #(not (some (partial = %) chars))))

;; If we want to parse a whole string (rather than just a character),
;; we can do so with parse-string.
(defn parse-string
  "Parse the given string."
  [string]
  (if-let [c (first string)]
    (parse
     (expect-char c)
     [rest (parse-string (rest string))]
     (constant (apply str c rest)))
    (constant '())))
