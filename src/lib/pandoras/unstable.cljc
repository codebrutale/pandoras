(ns pandoras.unstable
  #?(:cljs (:require-macros pandoras.unstable)))

(defmacro void
  [& body]
  `(do ~@body nil))

(defmacro non-nil
  "Evaluates the forms in left-to-right order returning the first non-nil value
  and short-circuiting the evaluation.  Each form is evaluated at most once.

  Examples:

    (non-nil) => nil
    (non-nil nil true) => true
    (non-nil false true) => false"
  ([] nil)
  ([x] x)
  ([x & xs]
   `(let [v# ~x]
      (if (some? v#) v# (non-nil ~@xs)))))

(defmacro select->
  "Threads `x` syntactically through the left-hand side of `forms` and evaluates
  the forms in a short-circuiting manner returning the first non-nil result,
  if any.  The expression `x` is evaluated at most once.

  Examples:

    (select-> true) => nil
    (select-> nil identity nil? boolean) => true
    (select-> nil identity boolean nil?) => false

    (select-> {:foo 1, :bar 2} :xyzzy :bar :foo) => 2
    (select-> {:foo 1, :bar 2}
      (get :xyzzy)
      (get :bar)
      (get :foo)) => 2"
  [x & forms]
  (let [v (gensym)]
    `(let [~v ~x]
       (non-nil ~@(map (fn [form]
                         (if (seq? form)
                           (let [f `(~(first form) ~v ~@(next form))]
                             (if-let [m (meta form)]
                               (with-meta f m)
                               f))
                           (list form v)))
                       forms)))))

;; XXX(soija): Hard to come up with useful examples for this one.  Maybe trash
;; this.
(defmacro select->>
  "Threads `x` syntactically through the right-hand side of `forms` and
  evaluates the forms in a short-circuiting manner returning the first non-nil
  result, if any.  The expression `x` is evaluated at most once."
  [x & forms]
  (let [v (gensym)]
    `(let [~v ~x]
       (non-nil ~@(map (fn [form]
                         (if (seq? form)
                           (let [f `(~(first form) ~@(next form) ~v)]
                             (if-let [m (meta form)]
                               (with-meta f m)
                               f))
                           (list form v)))
                       forms)))))

#_
(defmacro select-by->
  [f x & forms])

#_
(defmacro select-by->>
  "Threads `x` syntactically through the right-hand side of `forms` and
  evaluates the forms in a short-circuiting manner returning the first result
  that evaluates to a truth when applied to `f`.  The expression `x` is
  evaluated at most once.

  Examples:

    (select-by->> not-empty #{:foo :bar :baz}
      (set/intersection #{:xyzzy :yzxxz})
      (set/intersection #{:foo :bar :fizz :buzz})
    => #{:foo :bar}

    (select-by->> #(-> % :status (= 200)) params
      (http/get \"http://example.com/a\")
      (http/get \"http://example.com/b\")
      (http/get \"http://example.com/c\"))
    => {:status 200 :body ...}"
    [f x & forms])
