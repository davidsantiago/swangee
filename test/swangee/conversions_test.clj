(ns swangee.conversions-test
  (:use clojure.test
        swangee.core-test
        [swangee nfa dfa conversions])
  (:require [swangee.core :as swangee]))

(def test-nfa (nfa :states [:1 :2 :3]
                   :transitions {:1 {\a :2}
                                 :2 {\b #{:3}}
                                 :3 {\c :3
                                     \b #{:2}}}
                   :initial-state #{:1}
                   :accepting-states #{:3}))

(deftest determinize-test
  (is (= (swangee/run test-nfa "abccbb")
         (swangee/run (determinize test-nfa) "abccbb"))))