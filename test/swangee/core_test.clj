(ns swangee.core-test
  (:use clojure.test)
  (:require [swangee.core :as swangee]))


;;
;; Core operations (FA-independent, actual tests in nfa/dfa tests)
;;
;; For these tests, we use the simple language defined by the
;; regular expression "ab(bb|c)*".

(defmacro test-basic-run
  [fa accepted-strings rejected-strings]
  (let [test-name (gensym "basic-run")]
    `(deftest ~test-name
       ;; Basic run of entire string expecting success.
       (doseq [s# ~accepted-strings]
         (is (= true
                (swangee/run ~fa s#))))

       ;; Basic run of entire string that is not in the language.
       (doseq [s# ~rejected-strings]
         (is (= false
                (swangee/run ~fa s#)))))))

(defmacro test-basic-match
  [fa string-matches] ;; string-matches seq of pairs of strings and matches.
  (let [test-name (gensym "basic-match")]
    `(deftest ~test-name
       (doseq [[string# match#] ~string-matches]
         (is (= match#
                (swangee/match ~fa string#)))))))

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





