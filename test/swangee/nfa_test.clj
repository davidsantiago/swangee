(ns swangee.nfa-test
  (:use clojure.test
        [swangee core-test test-automata-defs nfa])
  (:require [swangee.core :as swangee]))

;;
;; Data definitions
;;

(def transitions-with-eps {:a {\b :b
                               nil :b}
                           :b {\d :d
                               \c :c
                               nil #{:c}}
                           :c {}
                           :d {nil #{:b :c}}})

;; Test the espilon-closure function.

(deftest epsilon-closure-test
    ;; Single states.
  (is (= #{:a :b :c}
         (epsilon-closure transitions-with-eps :a)))
  (is (= #{:b :c}
         (epsilon-closure transitions-with-eps :b)))
  (is (= #{:c}
         (epsilon-closure transitions-with-eps :c)))
  (is (= #{:b :c :d}
         (epsilon-closure transitions-with-eps :d)))
  ;; Single states as sets.
  (is (= #{:a :b :c}
         (epsilon-closure transitions-with-eps #{:a})))
  (is (= #{:b :c}
         (epsilon-closure transitions-with-eps #{:b})))
  (is (= #{:c}
         (epsilon-closure transitions-with-eps #{:c})))
  (is (= #{:b :c :d}
         (epsilon-closure transitions-with-eps #{:d})))
  ;; Sets of states
  (is (= #{:a :b :c}
         (epsilon-closure transitions-with-eps #{:a :b})))
  (is (= #{:b :c}
         (epsilon-closure transitions-with-eps #{:b :c})))
  (is (= #{:b :c :d}
         (epsilon-closure transitions-with-eps #{:d})))
  (is (= #{:a :b :c :d}
         (epsilon-closure transitions-with-eps #{:a :b :c :d}))))

(deftest epsilon-closure-nfas
  ;; lang1-nfa
  (is (= #{:1} (epsilon-closure (:transitions lang1-nfa) :1)))
  (is (= #{:2} (epsilon-closure (:transitions lang1-nfa) :2)))
  (is (= #{:3} (epsilon-closure (:transitions lang1-nfa) :3)))
  ;; lang2-nfa
  (is (= #{:q0 :q1 :q2} (epsilon-closure (:transitions lang2-nfa) :q0)))
  (is (= #{:q1 :q2} (epsilon-closure (:transitions lang2-nfa) :q1)))
  (is (= #{:q2} (epsilon-closure (:transitions lang2-nfa) :q2)))
  ;; lang3-nfa
  (is (= #{:1 :2 :4} (epsilon-closure (:transitions lang3-nfa) :1)))
  (is (= #{:2} (epsilon-closure (:transitions lang3-nfa) :2)))
  (is (= #{:3 :6} (epsilon-closure (:transitions lang3-nfa) :3)))
  (is (= #{:4} (epsilon-closure (:transitions lang3-nfa) :4)))
  (is (= #{:5 :6} (epsilon-closure (:transitions lang3-nfa) :5)))
  (is (= #{:6} (epsilon-closure (:transitions lang3-nfa) :6)))
  (is (= #{:1 :2 :4} (epsilon-closure (:transitions lang3-nfa) #{:1}))))

;; See core_test.clj for the language being tested.

(deftest simple-step-nfa
  ;; Start at initial configuration and step once.
  (is (= (swangee/config #{:2} (seq "bccb"))
         (swangee/step lang1-nfa (swangee/config (:initial-state lang1-nfa)
                                                "abccb"))))
  ;; Try another step.
  (is (= (swangee/config #{:3} (seq "ccb"))
         (swangee/step lang1-nfa (swangee/config #{:2}
                                                "bccb"))))

  ;; Undefined input.
  (is (= (swangee/config #{} (seq "ddd"))
         (swangee/step lang1-nfa (swangee/config #{:1} "dddd")))))

;; Run the core operations tests on this nfa.
(test-basic-run lang1-nfa lang1-strings not-lang1-strings)
(test-basic-match lang1-nfa lang1-string-matches)
(test-complement lang1-nfa)

;;
;; Test an NFA with epsilon-transitions on the language a*b*c*.
;;

(deftest simple-step-e-nfa
  ;; Start at initial configuration and step once.
  (is (= (swangee/config #{:q0} (seq "bc"))
         (swangee/step lang2-nfa (swangee/config (:initial-state lang2-nfa)
                                                  "abc"))))
  (is (= (swangee/config #{:q1} (seq "c"))
         (swangee/step lang2-nfa (swangee/config #{:q1}
                                                  "bc"))))
  (is (= (swangee/config #{:q2} '())
         (swangee/step lang2-nfa (swangee/config #{:q2}
                                                  "c")))))

;;
;; Test NFA-specific operations
;;

(deftest simple-move-nfa
    (is (= #{:2}
           (move-nfa lang1-nfa #{:1} \a)))
    (is (= #{}
           (move-nfa lang1-nfa #{:1} \b)))
    (is (= #{:q0}
           (move-nfa lang2-nfa #{:q0} \a)))
    (is (= #{:q1}
           (move-nfa lang2-nfa #{:q0} \b)))
    (is (= #{:2 :5}
           (move-nfa lang3-nfa #{:1} \b)))
    (is (= #{:3 :4}
           (move-nfa lang3-nfa #{:1} \a))))

(deftest simple-outgoing-symbols
  (is (= #{\a}
         (outgoing-symbols lang1-nfa #{:1})))
  (is (= #{\b \c}
         (outgoing-symbols lang1-nfa #{:3})))
  (is (= #{\a}
         (outgoing-symbols lang2-nfa #{:q0})))
  (is (= #{}
         (outgoing-symbols lang3-nfa #{:6}))))
