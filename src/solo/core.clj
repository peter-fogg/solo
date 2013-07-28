(ns solo.core)

(defn satisfy
  "Returns a parser accepting a character satisfying `pred`."
  [pred]
  (fn [state]
    (let [c (first state)]
      (when (and c (pred c))
        {:val c
         :rest (apply str (rest state))}))))

(def parse-char (satisfy (fn [_] true)))

(defn constant
  "The constant parser."
  [val]
  (fn [state]
    {:val val
     :rest state}))

(defn map-parser
  "Map `f` across the result of `parser`."
  [f parser]
  (fn [state]
    (when-let [result (parser state)]
      (assoc result :val (f (:val result))))))

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

(defn chain-parser
  "Combine two parsers into a new one."
  [left right]
  (fn [state]
    (when-let [{val :val new-state :rest} (left state)]
      ((right val) new-state))))

(defn get-state
  "Get the current state of the parser."
  [state]
  {:val state :rest state})

(defn put-state
  "Modify the state of the parser."
  [state]
  (fn [_]
    {:val '() :rest state}))

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

(defn expect-char
  "Expect `char`, and fail if not present."
  [char]
  (satisfy (partial = char)))

(defn parse-or
  "Choice of several parsers."
  [& parsers]
  (fn [state]
    (some #(% state) parsers)))

(defn parse-maybe
  "Option parser. Return a nil value if `parser` fails."
  [parser]
  (fn [state]
    (when-not (empty? state)
      (if-let [result (parser state)]
        result
        {:val nil
         :rest state}))))

(defn parse-many1
  "Repeatedly apply `parser`, requiring that it parse at least once."
  [parser]
  (parse
   [val parser]
   [rest (parse-or (parse-many1 parser) (constant []))]
   (constant (cons val rest))))

(defn parse-many
  [parser]
  (parse
   [first (parse-maybe parser)]
   [rest (parse-or (parse-many1 parser) (constant []))]
   (constant (if first
               (cons first rest)
               rest))))

(defn one-of
  "Parse any of the supplied characters."
  [chars]
  (satisfy #(some (partial = %) chars)))

(defn not-char
  "Reject the supplied character."
  [char]
  (fn [state]
    (when-not (= char (first state))
      {:val (str (first state))
       :rest (apply str (rest state))})))

(defn none-of
  "Reject any of the supplied characters."
  [chars]
  (satisfy #(not (some (partial = %) chars))))

(defn parse-string
  "Parse the given string."
  [string]
  (if-let [c (first string)]
    (parse
     (expect-char c)
     [rest (parse-string (rest string))]
     (constant (apply str c rest)))
    (constant '())))

(defn sep-by
  "Parse a list of `left`, separated by `right`."
  [left right]
  (parse
   [first left]
   [rest (parse-or
          (parse right (sep-by left right))
          (constant '()))]
   (constant (cons first rest))))

(defn parse-n
  "Run `parser` n times, returning a list."
  [n parser]
  (if (zero? n)
    (constant '())
    (parse
     [thing parser]
     (map-parser #(cons thing %) (parse-n (dec n) parser)))))
