(ns swangee.nfa-test
  (:use clojure.test
        swangee.core-test
        swangee.nfa)
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

;; NFA for the language ab(bb|c)*
(def test-nfa (nfa :states [:1 :2 :3]
                   :transitions {:1 {\a :2}
                                 :2 {\b #{:3}}
                                 :3 {\c :3
                                     \b #{:2}}}
                   :initial-state #{:1}
                   :accepting-states #{:3}))

;; NFA for the language a*b*c*
(def test-e-nfa (nfa :states [:q0 :q1 :q2]
                     :transitions {:q0 {\a :q0
                                        nil :q1}
                                   :q1 {\b :q1
                                        nil :q2}
                                   :q2 {\c :q2}}
                     :initial-state #{:q0}
                     :accepting-states #{:q2}))

;; NFA for the language (b*a)|(a*b).
(def test-move-nfa (nfa :states [:1 :2 :3 :4 :5 :6]
                        :transitions {:1 {nil #{:2 :4}}
                                      :2 {\b :2
                                          \a :3}
                                      :3 {nil :6}
                                      :4 {\a :4
                                          \b :5}
                                      :5 {nil :6}
                                      :6 {}}
                        :initial-state #{:1}
                        :accepting-states #{:6}))

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
  ;; test-nfa
  (is (= #{:1} (epsilon-closure (:transitions test-nfa) :1)))
  (is (= #{:2} (epsilon-closure (:transitions test-nfa) :2)))
  (is (= #{:3} (epsilon-closure (:transitions test-nfa) :3)))
  ;; test-e-nfa
  (is (= #{:q0 :q1 :q2} (epsilon-closure (:transitions test-e-nfa) :q0)))
  (is (= #{:q1 :q2} (epsilon-closure (:transitions test-e-nfa) :q1)))
  (is (= #{:q2} (epsilon-closure (:transitions test-e-nfa) :q2)))
  ;; test-move-nfa
  (is (= #{:1 :2 :4} (epsilon-closure (:transitions test-move-nfa) :1)))
  (is (= #{:2} (epsilon-closure (:transitions test-move-nfa) :2)))
  (is (= #{:3 :6} (epsilon-closure (:transitions test-move-nfa) :3)))
  (is (= #{:4} (epsilon-closure (:transitions test-move-nfa) :4)))
  (is (= #{:5 :6} (epsilon-closure (:transitions test-move-nfa) :5)))
  (is (= #{:6} (epsilon-closure (:transitions test-move-nfa) :6)))
  (is (= #{:1 :2 :4} (epsilon-closure (:transitions test-move-nfa) #{:1})))
  )

;; See core_test.clj for the language being tested.

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

;;
;; Test NFA-specific operations
;;

(deftest simple-move-nfa
    (is (= #{:2}
           (move-nfa test-nfa #{:1} \a)))
    (is (= #{}
           (move-nfa test-nfa #{:1} \b)))
    (is (= #{:q0}
           (move-nfa test-e-nfa #{:q0} \a)))
    (is (= #{:q1}
           (move-nfa test-e-nfa #{:q0} \b)))
    (is (= #{:2 :5}
           (move-nfa test-move-nfa #{:1} \b)))
    (is (= #{:3 :4}
           (move-nfa test-move-nfa #{:1} \a))))

(deftest simple-outgoing-symbols
  (is (= #{\a}
         (outgoing-symbols test-nfa #{:1})))
  (is (= #{\b \c}
         (outgoing-symbols test-nfa #{:3})))
  (is (= #{\a}
         (outgoing-symbols test-e-nfa #{:q0})))
  (is (= #{}
         (outgoing-symbols test-move-nfa #{:6}))))
