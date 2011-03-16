(ns swangee.nfa-test
  (:use clojure.test
        swangee.core-test
        swangee.nfa)
  (:require [swangee.core :as swangee]))

;; Test the espilon-closure function.

(def transitions-with-eps {:a {\b :b
                               nil :b}
                           :b {\d :d
                               \c :c
                               nil #{:c}}
                           :c {}
                           :d {nil #{:b :c}}})

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

;; See core_test.clj for the language being tested.

(def test-nfa (nfa :states [:1 :2 :3]
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

;; Run the core operations tests on this nfa.
(test-basic-run test-nfa)
(test-basic-match test-nfa)
(test-complement test-nfa)

;;
;; Test an NFA with epsilon-transitions on the language a*b*c*.
;;

(def test-e-nfa (nfa :states [:q0 :q1 :q2]
                     :transitions {:q0 {\a :q0
                                        nil :q1}
                                   :q1 {\b :q1
                                        nil :q2}
                                   :q2 {\c :q2}}
                     :initial-state #{:q0}
                     :accepting-states #{:q2}))

(deftest simple-step-e-nfa
  ;; Start at initial configuration and step once.
  (is (= (swangee/config #{:q0} (seq "bc"))
         (swangee/step test-e-nfa (swangee/config (:initial-state test-e-nfa)
                                                  "abc"))))
  (is (= (swangee/config #{:q1} (seq "c"))
         (swangee/step test-e-nfa (swangee/config #{:q1}
                                                  "bc"))))
  (is (= (swangee/config #{:q2} '())
         (swangee/step test-e-nfa (swangee/config #{:q2}
                                                  "c")))))



