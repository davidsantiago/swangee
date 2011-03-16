(ns swangee.core
  (:refer-clojure :exclude [complement])
  (:require [clojure.set :as set])
  (:use swangee.utils))

(defrecord Configuration [state   ;; Automaton-defined state.
                          input]) ;; Remaining part of the input tape.

(defn config
  "Convenience constructor for Configuration."
  [state input]
  (Configuration. state input))

(defprotocol FiniteAutomaton
  "A runnable automaton."
  (initial-state [this]
                 "Returns the initial state of this automaton, whatever the
                  state of this type of automaton happens to be.")
  (accepting-state? [this state] "Returns true if the passed parameter is
                                  one that would result in acceptance.")
  (valid-state? [this state] "Returns true if the passed parameter is considered
                              a valid state by this machine.")
  (step [this ^Configuration config]
        "Takes FA and a Configuration, yield the next Configuration."))

(defprotocol ComposableAutomaton
  "Automata that implement this protocol are cable of performing the following
   operations (that is, returning a new automaton that has new but related
   behavior). This is useful for building up complex automata out of simple ones."
  (complement [this]
    "Given a FiniteAutomaton, return a FiniteAutomaton that is its complement."))

;; Main difference between DFA and NFA is the state concept. A DFA uses
;; singleton states, an NFA uses sets for states.
;; Transition function. NFA: T : Q x E -> P(Q)  (singleton or collection)
;;                      DFA: T : Q x E -> Q
;; Initial state. NFA: I = #{x,...}
;;                DFA: I = x

;;
;; Core operations
;;

(defn run
  "Given a FiniteAutomaton and an input sequence, returns true (accept) or false."
  [^FiniteAutomaton fa input]
  (loop [curr-cfg (config (initial-state fa)
                          input)]
    (let [next-cfg (step fa curr-cfg)]
      (if (not (valid-state? fa (:state next-cfg))) ;; Was no valid transition...
        false
        (if (empty? (:input next-cfg)) ;; Ran out of input, return based on final state.
          (accepting-state? fa (:state next-cfg))
          (recur next-cfg)))))) ;; Still have input, so recur.

(defn match
  "Given a FiniteAutomaton and an input sequence, return the longest string matched
   starting on the first character of the string."
  [^FiniteAutomaton fa input]
  (loop [curr-cfg (config (initial-state fa)
                          input)
         symbols-seen [(first input)]
         longest-match []]
    (let [next-cfg (step fa curr-cfg)]
      (if (not (valid-state? fa (:state next-cfg))) ;; No route to acceptance...
        longest-match
        (if (empty? (:input next-cfg)) ;; Out of input, return depends on state.
          (if (accepting-state? fa (:state next-cfg)) ;; If we are in accepting state,
            (conj longest-match (first (:input curr-cfg))) ;; Add last input we saw and return.
            ;; Otherwise, we are out of input but not in accepting, state...
            longest-match)
          ;; Not out of input, so continue...
          (recur next-cfg
                 (conj symbols-seen (first (:input next-cfg)))
                 ;; Only set longest-match to symbols seen if we're in accepting state.
                 (if (accepting-state? fa (:state next-cfg))
                   symbols-seen
                   longest-match)))))))

