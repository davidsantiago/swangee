(ns swangee.conversions
  (:refer-clojure :exclude [complement])
  (:use [swangee core nfa dfa])
  (:require [swangee.core :as swangee]
            [clojure.set :as set]))

(defn determinize
  "Converts the given NFA into a DFA using Buchi's (subset construction)
   algorithm. For example, see Aho, Sethi & Ullman Algorithm 3.2 or
   http://web.cecs.pdx.edu/~harry/compilers/slides/LexicalPart3.pdf"
  [nfa]
  ;; First save the "name" (a set of nfa states) of the initial state
  ;; for the final call to make the dfa.
  (let [initial-state (epsilon-closure (:transitions nfa)
                                       (:initial-state nfa))]
    (loop [dfa-states {} ;; Map of sets of nfa states to compiled-states
           unmarked-states #{initial-state}] ;; Set of sets of nfa states.
      (if-let [dfa-state (first unmarked-states)] ;; dfa-state is a set.
        (let [dfa-state-trans
              (into {} (for [sym (outgoing-symbols nfa dfa-state)]
                         ;; For every symbol that has a transition out, add
                         ;; a transition to the set of possible nfa states that
                         ;; the nfa is in after seeing that input.
                         [sym (epsilon-closure (:transitions nfa)
                                               (move-nfa nfa dfa-state sym))]))
              ;; Will set this dfa state to be final if any primitive nfa state
              ;; is final.
              accepting? (accepting-state? nfa dfa-state)]
           (recur (conj dfa-states [dfa-state (compiled-state dfa-state-trans
                                                             accepting?)])
                 ;; Only add into unmarked-states the new states we built as
                 ;; sets of possible destinations that haven't already been
                 ;; added to the final dfa. Final DFA state "names" are sets
                 ;; in the keys of the dfa-states map, and values in the
                 ;; map of this particular new state's transitions map.
                 (set/union (rest unmarked-states)
                            (set/difference (apply hash-set
                                                   (vals dfa-state-trans))
                                            (apply hash-set (keys dfa-states))))))
        ;; if-let failed: ran out of unmarked states. Finish up and return.
        (dfa :states dfa-states :initial-state initial-state)))))
