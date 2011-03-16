(ns swangee.core-test
  (:use clojure.test)
  (:require [swangee.core :as swangee]))


;;
;; Core operations (FA-independent, actual tests in nfa/dfa tests)
;;
;; For these tests, we use the simple language defined by the
;; regular expression "ab(bb|c)*".

(defmacro test-basic-run
  [fa]
  (let [test-name (gensym "basic-run")]
    `(deftest ~test-name
       ;; Basic run of entire string expecting success
       (is (= true
              (swangee/run ~fa "abccbb")))

       ;; Basic run of entire string with characters not in any transition.
       (is (= false
              (swangee/run ~fa "abddcba"))))))

(defmacro test-basic-match
  [fa]
  (let [test-name (gensym "basic-match")]
    `(deftest ~test-name
       ;; Basic match of string prefix expecting success
       (is (= (seq "abc")
              (swangee/match ~fa "abcd")))

       ;; Basic match of string prefix expecting failure
       (is (= []
                (swangee/match ~fa "dbbabc")))

       ;; Basic match of string prefix consuming entire string
       (is (= (seq "abccbb"))
           (swangee/match ~fa "abccbb"))

       ;; Basic match of string with multiple matches
       (is (= (seq "abccbbc"))
           (swangee/match ~fa "abccbbc")))))

(defmacro test-complement
  [fa]
  (let [test-name (gensym "test-complement")]
    `(deftest ~test-name
       ;; Try a string in the complement of the language.
       (is (= true
              (swangee/run (swangee/complement ~fa) "abb")))
       ;; A string that is in the language should fail
       (is (= false
              (swangee/run (swangee/complement ~fa) "abccbbccc")))
       ;; Round trip through two complements, accept a string from original
       ;; language.
       (is (= true
              (swangee/run (swangee/complement (swangee/complement ~fa))
                           "abccbb"))))))





