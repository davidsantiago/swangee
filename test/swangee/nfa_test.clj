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
  (is (= #{:a :b :c}
         (epsilon-closure transitions-with-eps
                                  #{:a})))
  (is (= #{:b :c}
         (epsilon-closure transitions-with-eps
                                  #{:b})))
  (is (= #{:c}
         (epsilon-closure transitions-with-eps
                                  #{:c})))
  (is (= #{:b :c :d}
         (epsilon-closure transitions-with-eps
                                  #{:d}))))

;; For these tests, we use the simple language defined by the
;; regular expression "ab(bb|c)*".

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


;;
;; Core operation tests
;;

(test-basic-run test-nfa)
(test-basic-match test-nfa)
(test-complement test-nfa)