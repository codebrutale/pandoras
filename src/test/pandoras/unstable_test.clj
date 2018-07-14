(ns pandoras.unstable-test
  (:require [clojure.test :refer [deftest testing is]]
            [pandoras.unstable :refer :all]))

(deftest void-tests
  (testing "always returns nil"
    (is (= (void) nil))
    (is (= (void 1) nil))
    (is (= (void 1 2) nil))
    (is (= (void 1 2 3) nil))))

(deftest non-nil-tests
  (testing "(non-nil x) should be x for all x"
    (is (= (non-nil nil) nil))
    (is (= (non-nil true) true))
    (is (= (non-nil false) false))
    (is (= (non-nil 1) 1))
    (is (= (non-nil "") ""))
    (is (= (non-nil 'sym) 'sym))
    (is (= (non-nil :kw) :kw))
    (is (= (non-nil '()) '()))
    (is (= (non-nil []) []))
    (is (= (non-nil {}) {}))
    (is (= (non-nil #{}) #{}))
    (let [e (Exception. "")]
      (is (= (non-nil e) e))))
  (testing "stops at first non-nil x"
    (is (= (non-nil nil nil :reached) :reached))
    (is (= (non-nil nil true :not-reached) true))
    (is (= (non-nil nil false :not-reached) false))
    (is (= (non-nil nil 1 :not-reached) 1))
    (is (= (non-nil nil "" :not-reached) ""))
    (is (= (non-nil nil 'sym :not-reached) 'sym))
    (is (= (non-nil nil :kw :not-reached) :kw))
    (is (= (non-nil nil '() :not-reached) '()))
    (is (= (non-nil nil [] :not-reached) []))
    (is (= (non-nil nil {} :not-reached) {}))
    (is (= (non-nil nil #{} :not-reached) #{}))
    (let [e (Exception. "")]
      (is (= (non-nil e :not-reached) e))))
  (testing "works over different arities"
    (is (= (non-nil nil 2) 2))
    (is (= (non-nil nil nil 3) 3))
    (is (= (non-nil nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil
                    65) 65)))
  (testing "preserves meta"
    ;; Apparently testing macros don't preserve meta :/
    (letfn [(helper [n]
              (meta (case n
                      1 (non-nil ^:foo [])
                      2 (non-nil nil ^:foo [])
                      3 (non-nil nil nil ^:foo []))))]
      (is (= (helper 1) {:foo true}))
      (is (= (helper 2) {:foo true}))
      (is (= (helper 3) {:foo true}))))
  (testing "evaluates minimally"
    (let [s (atom [])
          a-nil (fn [x] (swap! s conj x) nil)
          a-non-nil (fn [x] (swap! s conj x) @s)]
      (is (= (non-nil (a-nil 1) (a-nil 2) (a-nil 3)
                      (a-non-nil 4)
                      (a-nil 4) (a-nil 5) (a-nil 6))
             [1 2 3 4])))))

(deftest select->tests
  (testing "(select-> x) should always be nil"
    (is (= (select-> nil) nil))
    (is (= (select-> true) nil))
    (is (= (select-> false) nil))
    (is (= (select-> 1) nil))
    (is (= (select-> "") nil))
    (is (= (select-> 'sym) nil))
    (is (= (select-> :kw) nil))
    (is (= (select-> '()) nil))
    (is (= (select-> []) nil))
    (is (= (select-> {}) nil))
    (is (= (select-> #{}) nil))
    (let [e (Exception. "")]
      (is (= (select-> e) nil))))
  (testing "works over different arities and threads left"
    (let [a-nil (constantly nil)]
      (is (= (select-> 1
               (vector :a :b :c))
             [1 :a :b :c]))
      (is (= (select-> 2
               a-nil
               (vector :a :b :c))
             [2 :a :b :c]))
      (is (= (select-> 3
               a-nil a-nil
               (vector :a :b :c))
             [3 :a :b :c]))
      (is (= (select-> 65
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               (vector :a :b :c))
             [65 :a :b :c]))))
  (testing "preserves meta"
    (let [f (fn [] ^:foo [])]
      (is (= (select-> (f) meta) {:foo true}))))
  (testing "evaluates minimally"
    (let [s (atom {:arg 0 :fn []})
          f (fn [] (-> (swap! s update :arg inc) :arg))
          a-nil (fn [_ k] (swap! s update :fn conj k) nil)
          a-non-nil (fn [_ k] (swap! s update :fn conj k) @s)]
      (is (= (select-> (f)
               (a-nil :a)
               (a-nil :b)
               (a-non-nil :c)
               (a-non-nil :d)
               (a-nil :e))
             {:arg 1
              :fn [:a :b :c]})))))

(deftest select->>tests
  (testing "(select->> x) should always be nil"
    (is (= (select->> nil) nil))
    (is (= (select->> true) nil))
    (is (= (select->> false) nil))
    (is (= (select->> 1) nil))
    (is (= (select->> "") nil))
    (is (= (select->> 'sym) nil))
    (is (= (select->> :kw) nil))
    (is (= (select->> '()) nil))
    (is (= (select->> []) nil))
    (is (= (select->> {}) nil))
    (is (= (select->> #{}) nil))
    (let [e (Exception. "")]
      (is (= (select->> e) nil))))
  (testing "works over different arities and threads left"
    (let [a-nil (constantly nil)]
      (is (= (select->> 1
               (vector :a :b :c))
             [:a :b :c 1]))
      (is (= (select->> 2
               a-nil
               (vector :a :b :c))
             [:a :b :c 2]))
      (is (= (select->> 3
               a-nil a-nil
               (vector :a :b :c))
             [:a :b :c 3]))
      (is (= (select->> 65
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               a-nil a-nil a-nil a-nil a-nil a-nil a-nil a-nil
               (vector :a :b :c))
             [:a :b :c 65]))))
  (testing "preserves meta"
    (let [f (fn [] ^:foo [])]
      (is (= (select->> (f) meta) {:foo true}))))
  (testing "evaluates minimally"
    (let [s (atom {:arg 0 :fn []})
          f (fn [] (->> (swap! s update :arg inc) :arg))
          a-nil (fn [k _] (swap! s update :fn conj k) nil)
          a-non-nil (fn [k _] (swap! s update :fn conj k) @s)]
      (is (= (select->> (f)
               (a-nil :a)
               (a-nil :b)
               (a-non-nil :c)
               (a-non-nil :d)
               (a-nil :e))
             {:arg 1
              :fn [:a :b :c]})))))
