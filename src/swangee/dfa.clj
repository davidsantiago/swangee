(ns swangee.dfa
  (:refer-clojure :exclude [complement])
  (:require [clojure.set :as set])
  (:use [swangee core utils]))

;; NFAs are much nicer and easier to work with, but DFAs are much faster to
;; simulate. You can, of course, express a DFA as an NFA, so for building and
;; messing around, an NFA is usually best. Convert to a DFA if you want. But we
;; need a lower-level DFA construct.
;;
;; In our DFA, a state is an object containing its outbound transitions and
;; a flag indicating whether it is an accepting state. A transition is a function
;; of an input that return the next state. We don't accept a list of such, we
;; only work with a "compiled" transition function that we apply once to get
;; the answer. Again, no assumption of such, but a map of inputs to states is
;; a great, fast way to do this that remains easily modifiable. Also, note that
;; a transition returns the state object itself, not its name.

;; A state object that contains its transitions (function of input only)
;; and a flag for whether this is an accepting state.
(defrecord CompiledState [transitions accepting?])

(defn compiled-state
  "A convenient constructor for a compiled state."
  [transitions accepting?]
  (CompiledState. transitions accepting?))

;; For a DFA:
;;   Transition function (E -> Q). Only returns a state.
;;   Initial state: A single initial state.
;;   Configuration: A state and input sequence.

(defrecord DFA [states             ;; A map of 'names' (whatever) to states.
                initial-state]     ;; A single initial state, a member of states.
  FiniteAutomaton
  (initial-state [this] (:initial-state this))
  (accepting-state? [this state] (boolean (:accepting? ((:states this) state))))
  (valid-state? [this state] (contains? (:states this) state))
  (step [this {:keys [state input]}]
        (config ((or (:transitions ((:states this) state)
                                   (constantly nil))) (first input))
                (rest input))))

(defn dfa
  "Takes arguments for states, and initial state to construct
   a dfa."
  [& rest]
  (let [{:keys [states initial-state]} (apply hash-map rest)]
    (DFA. states
          initial-state)))
