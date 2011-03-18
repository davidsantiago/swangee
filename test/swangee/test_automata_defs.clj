(ns swangee.test-automata-defs
  "Definitions of automata for use in other tests."
  (:use [swangee nfa dfa]))

;;
;; Language 1
;;
;; The language ab(bb|c)*. NFA has no epsilon transitions.
(def lang1-nfa (nfa :states [:1 :2 :3]
                    :transitions {:1 {\a :2}
                                  :2 {\b #{:3}}
                                  :3 {\c :3
                                      \b #{:2}}}
                    :initial-state #{:1}
                    :accepting-states #{:3}))

(def lang1-dfa (dfa :states {:1 (compiled-state {\a :2} false)
                             :2 (compiled-state {\b :3} false)
                             :3 (compiled-state {\c :3
                                                 \b :2} true)}
                    :initial-state :1))

(def lang1-strings ["abccbb" "ab" "abc" "abbb" "abbbccc" "abccbb"])
(def not-lang1-strings ["a" "b" "aba" "abac" "abbc" "abbbb" "abbcb"
                        "abbbbccc" "abddcba" "dfa"])
(def lang1-string-matches [["" []]
                           ["adbc" []]
                           ["abcd" (seq "abc")]
                           ["dbbabc" []]
                           ["ab" (seq "ab")]
                           ["abccbb" (seq "abccbb")]
                           ["abccbbc" (seq "abccbbc")]])

;;
;; Language 2
;;
;; The language a*b*c*.
(def lang2-nfa (nfa :states [:q0 :q1 :q2]
                    :transitions {:q0 {\a :q0
                                       nil :q1}
                                  :q1 {\b :q1
                                       nil :q2}
                                  :q2 {\c :q2}}
                    :initial-state #{:q0}
                    :accepting-states #{:q2}))

(def lang2-strings ["" "a" "b" "c" "aa" "bb" "cc" "abc" "aabbc" "aabbcc"])
(def not-lang2-strings ["aba" "abac" "dfa" "cccb"])

;;
;; Language 3
;;
;; The language (b*a)|(a*b).
(def lang3-nfa (nfa :states [:1 :2 :3 :4 :5 :6]
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

(def lang3-strings ["a" "b" "aa" "bb" "ba" "ab" "bbba" "aaaab"])
(def not-lang3-strings ["baa" "bab" "aaabb" "aba" "dfa"])