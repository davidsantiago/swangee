(ns swangee.conversions-test
  (:use clojure.test
        swangee.core-test
        [swangee test-automata-defs nfa dfa conversions])
  (:require [swangee.core :as swangee]))

;;
;; Test determinization of an NFA.
;;
(test-basic-run (determinize lang1-nfa) lang1-strings not-lang1-strings)
(test-basic-run (determinize lang2-nfa) lang2-strings not-lang2-strings)
(test-basic-run (determinize lang3-nfa) lang3-strings not-lang3-strings)

(test-basic-match (determinize lang1-nfa) lang1-string-matches)
(test-basic-match (determinize lang2-nfa) lang2-string-matches)
(test-basic-match (determinize lang3-nfa) lang3-string-matches)