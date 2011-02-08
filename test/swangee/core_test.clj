(ns swangee.core-test
  (:use clojure.test
        swangee.core))

(def test-nfa (nfa :states [:1 :2 :3]
                   :transitions [{:1 {\a :2}
                                  :2 {\b #{:3}}
                                  :3 {\c :3
                                      \b #{:2}}}]
                   :initial-state #{:1}
                   :accepting-states #{:3}))

(deftest simple-step-nfa
  ;; Start at initial configuration and step once.
  (is (= (config #{:2} (seq "bccb"))
         (step test-nfa (config (:initial-state test-nfa)
                                "abccb"))))
  ;; Try another step.
  (is (= (config #{:3} (seq "ccb"))
         (step test-nfa (config #{:2}
                                "bccb"))))

  ;; Undefined input.
  (is (= (config #{} (seq "ddd"))
         (step test-nfa (config #{:1} "dddd")))))

(deftest basic-run-nfa
  ;; Basic run of entire string expecting success
  (is (= true
         (run test-nfa "abccbb")))

  ;; Basic run of entire string with characters not in any transition.
  (is (= false
         (run test-nfa "abddcba"))))

;; Define the same DFA as above, as a DFA.
(def test-dfa (dfa :states {:1 (compiled-state {\a :2} false)
                            :2 (compiled-state {\b :3} false)
                            :3 (compiled-state {\c :3
                                                \b :2} true)}
                   :initial-state :1))

(deftest simple-step-dfa
  ;; Start at initial configuration and step once.
  (is (= (config :2 (seq "bccb"))
         (step test-dfa (config (:initial-state test-dfa)
                                "abccb"))))

  ;; Try another step.
  (is (= (config :3 (seq "ccb"))
         (step test-dfa (config :2 "bccb"))))

  ;; Undefined input.
  (is (= (config nil (seq "ddd"))
         (step test-dfa (config :1 "dddd")))))

(deftest basic-run-dfa
  ;; Basic run of entire string expecting success
  (is (= true
         (run test-dfa "abccbb")))

  ;; Basic run of entire string with characters not in any transition.
  (is (= false
         (run test-dfa "abddcba"))))


