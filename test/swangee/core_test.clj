(ns swangee.core-test
  (:use clojure.test)
  (:require [swangee.core :as swangee]))

;; For these tests, we use the simple language defined by the
;; regular expression "ab(bb|c)*".

(def test-nfa (swangee/nfa :states [:1 :2 :3]
                           :transitions {:1 {\a :2}
                                         :2 {\b #{:3}}
                                         :3 {\c :3
                                             \b #{:2}}}
                           :initial-state #{:1}
                           :accepting-states #{:3}))

(deftest simple-step-nfa
  ;; Start at initial configuration and step once.
  (is (= (swangee/config #{:2} (seq "bccb"))
         (swangee/step test-nfa (swangee/config (:initial-state test-nfa)
                                                "abccb"))))
  ;; Try another step.
  (is (= (swangee/config #{:3} (seq "ccb"))
         (swangee/step test-nfa (swangee/config #{:2}
                                                "bccb"))))

  ;; Undefined input.
  (is (= (swangee/config #{} (seq "ddd"))
         (swangee/step test-nfa (swangee/config #{:1} "dddd")))))

;; Define the same FA as above, as a DFA.
(def test-dfa (swangee/dfa :states {:1 (swangee/compiled-state {\a :2} false)
                                    :2 (swangee/compiled-state {\b :3} false)
                                    :3 (swangee/compiled-state {\c :3
                                                                \b :2} true)}
                           :initial-state :1))

(deftest simple-step-dfa
  ;; Start at initial configuration and step once.
  (is (= (swangee/config :2 (seq "bccb"))
         (swangee/step test-dfa (swangee/config (:initial-state test-dfa)
                                                "abccb"))))

  ;; Try another step.
  (is (= (swangee/config :3 (seq "ccb"))
         (swangee/step test-dfa (swangee/config :2 "bccb"))))

  ;; Undefined input.
  (is (= (swangee/config nil (seq "ddd"))
         (swangee/step test-dfa (swangee/config :1 "dddd")))))

;;
;; Core operations (FA-independent)
;;

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

(test-basic-run test-nfa)
(test-basic-run test-dfa)

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

(test-basic-match test-nfa)
(test-basic-match test-dfa)

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

(test-complement test-nfa)





