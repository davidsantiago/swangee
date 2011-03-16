(ns swangee.dfa-test
  (:use clojure.test
        swangee.dfa
        swangee.core-test)
  (:require [swangee.core :as swangee]))

;; For these tests, we use the simple language defined by the
;; regular expression "ab(bb|c)*".
(def test-dfa (dfa :states {:1 (compiled-state {\a :2} false)
                            :2 (compiled-state {\b :3} false)
                            :3 (compiled-state {\c :3
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
;; Core operations tests.
;;
(test-basic-run test-dfa)
(test-basic-match test-dfa)