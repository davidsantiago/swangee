(ns swangee.dfa-test
  (:use clojure.test
        [swangee core-test test-automata-defs dfa])
  (:require [swangee.core :as swangee]))

(deftest simple-step-dfa
  ;; Start at initial configuration and step once.
  (is (= (swangee/config :2 (seq "bccb"))
         (swangee/step lang1-dfa (swangee/config (:initial-state lang1-dfa)
                                                "abccb"))))

  ;; Try another step.
  (is (= (swangee/config :3 (seq "ccb"))
         (swangee/step lang1-dfa (swangee/config :2 "bccb"))))

  ;; Undefined input.
  (is (= (swangee/config nil (seq "ddd"))
         (swangee/step lang1-dfa (swangee/config :1 "dddd")))))


;;
;; Core operations tests.
;;
(test-basic-run lang1-dfa lang1-strings not-lang1-strings)
(test-basic-match lang1-dfa lang1-string-matches)