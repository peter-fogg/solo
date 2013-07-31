(ns solo.core)

(defn err
  "Attach an error message `msg` to `parser`."
  [parser msg]
  (fn [state]
    (let [result (parser state)]
      (if (:err result)
        (assoc result :err msg)
        result))))

(defn satisfy
  "Returns a parser accepting a character satisfying `pred`."
  [pred]
  (fn [{state :state pos :pos}]
    (let [c (first state)]
      (if (and c (pred c))
        {:val c
         :rest (apply str (rest state))
         :pos (inc pos)}
        {:pos pos
         :err "satisfy"}))))

(def parse-char (err (satisfy (fn [_] true)) "any character"))

(defn constant
  "The constant parser."
  [val]
  (fn [{state :state pos :pos}]
    {:val val
     :rest state
     :pos pos}))

(defn map-parser
  "Map `f` across the result of `parser`."
  [f parser]
  (fn [state]
    (let [result (parser state)]
      (if-let [val (:val result)]
        (assoc result :val (f val))
        result))))

(defn parse-while
  "Return a parser consuming as long as `pred` is true."
  [pred]
  (fn [{state :state pos :pos}]
    (let [val (apply str (take-while pred state))
          len (count val)
          rest (apply str (drop len state))]
      (if (empty? val)
        {:pos pos
         :err "parse-while"}
        {:val val
         :rest rest
         :pos (+ pos len)}))))

(defn chain-parser
  "Combine two parsers into a new one."
  [left right]
  (fn [{state :state pos :pos :as input}]
    (let [result (left input)]
      (if (:err result)
        result
        ((right (:val result)) {:state (:rest result) :pos (:pos result)})))))

(defn get-state
  "Get the current state of the parser."
  [{state :state pos :pos}]
  {:val state :rest state :pos pos})

(defn put-state
  "Modify the state of the parser."
  [state]
  (fn [{pos :pos}]
    {:val '() :rest state :pos pos}))

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
  (err (satisfy (partial = char)) (str "character '" char "'")))

(defn parse-or
  "Choice of several parsers."
  [& parsers]
  (fn [{state :state pos :pos :as input}]
    (let [results (map #(% input) parsers)]
      (if-let [result (first (filter #(not (nil? (:val %))) results))]
        result
        (first results)))))

(defn parse-maybe
  "Option parser. Return a nil value if `parser` fails."
  [parser]
  (fn [{state :state pos :pos :as input}]
    (if-not (empty? state)
      (let [result (parser input)]
        (if (:val result)
          result
          {:val nil
           :rest state
           :pos pos}))
      {:err "end of input"
       :pos pos})))

(defn parse-many-1
  "Repeatedly apply `parser`, requiring that it parse at least once."
  [parser]
  (parse
   [val parser]
   [rest (parse-or (parse-many-1 parser) (constant []))]
   (constant (cons val rest))))

(defn parse-many
  [parser]
  (parse
   [first (parse-maybe parser)]
   [rest (parse-or (parse-many-1 parser) (constant []))]
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
  (fn [{state :state pos :pos}]
    (if-not (= char (first state))
      {:val (first state)
       :rest (apply str (rest state))
       :pos (inc pos)}
      {:err (str "didn't expect '" char "'")
       :pos pos})))

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

(defn sep-by-1
  "Parse a list of at least one `left`, separated by `right`."
  [left right]
  (parse
   [first left]
   [rest (parse-or
          (parse right (sep-by-1 left right))
          (constant []))]
   (constant (cons first rest))))

(defn sep-by
  "Parse a list of `left`, separated by `right`."
  [left right]
  (parse
   [first (parse-maybe left)]
   ;; This seems quite messy to me, but seems necessary in order to
   ;; ensure that `right` doesn't consume any input.
   [rest (if first
           (parse-or (parse right (sep-by-1 left right))
                     (constant []))
           (constant []))]
   (constant (if first
               (cons first rest)
               []))))

(defn parse-n
  "Run `parser` n times, returning a list."
  [n parser]
  (if (zero? n)
    (constant '())
    (parse
     [thing parser]
     (map-parser #(cons thing %) (parse-n (dec n) parser)))))

(defn to-parse
  "Put a plain string into the right format for parsing."
  [string]
  {:state string :pos 0})
