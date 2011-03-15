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

;; For an NFA:
;;   Transition function (Q -> (E -> P(Q))). Can return a state or
;;   a collection of states. *Make sure this is a map of states to a map of
;;   input symbols to state/state collection*.
;;   Initial state: A state or collection of states.
;;   Configuration: Collection of states & input sequence.
(defrecord NFA [states ;; A collection of states
                transitions ;; A transition function (see above).
                initial-state ;; One of the identifiers in states.
                accepting-states] ;; A set of states in the state member.
  FiniteAutomaton
  (initial-state [this] (:initial-state this))
  (accepting-state? [this state]
                    (not (empty? (set/intersection state
                                                   (:accepting-states this)))))
  (valid-state? [this state] (not (empty? (set/intersection state
                                                            (:states this)))))
  (step [this {:keys [state input]}]
        ;; Have to apply transition to every primitive state in the state, then
        ;; apply all of those to the input symbol, collecting the answer states
        ;; together at the end. Calling hash-set just once is much faster than
        ;; set/union on a bunch of smaller hash-sets.
        (Configuration.
         (apply hash-set
                (apply concat (for [s state]
                                (as-coll
                                 ((or ((:transitions this) s)
                                      (constantly nil)) (first input))))))
         (rest input)))

  ComposableAutomaton
  (complement [this] (NFA. (:states this)
                           (:transitions this)
                           (:initial-state this)
                           ;; Next line is key: Turn all non-accepting states
                           ;; into accepting states and vice versa.
                           (set/difference (:states this)
                                           (:accepting-states this)))))

(defn nfa
  "Takes arguments for states, transitions, and initial state to construct
   an nfa."
  [& rest]
  (let [{:keys [states transitions initial-state accepting-states]}
        (apply hash-map rest)]
    (NFA. (apply hash-set states)
          transitions
          initial-state
          (apply hash-set accepting-states))))

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
        (Configuration. ((or (:transitions ((:states this) state)
                                           (constantly nil))) (first input))
                        (rest input))))

(defn dfa
  "Takes arguments for states, and initial state to construct
   a dfa."
  [& rest]
  (let [{:keys [states initial-state]} (apply hash-map rest)]
    (DFA. states
          initial-state)))

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

