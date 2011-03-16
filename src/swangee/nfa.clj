(ns swangee.nfa
  (:refer-clojure :exclude [complement])
  (:require [clojure.set :as set])
  (:use [swangee core utils]))

(defn epsilon-closure
  "Given an NFA's transition function (A map of states to maps of inputs to
   states), and a state, returns the epsilon closure of that state."
  [transitions state]
  (loop [e-closure state] ;; Start with the state itself.
    (let [e-reachable-states
          (apply concat
                 (filter #(not (nil? %))
                         (map #(as-coll (get (transitions %) nil))
                              e-closure)))
          next-e-closure (into e-closure e-reachable-states)]
      (if (= next-e-closure e-closure)
        e-closure
        (recur next-e-closure)))))

;; For an NFA:
;;   Transition function (Q -> (E -> P(Q))). Can return a state or
;;   a collection of states. *Make sure this is a map of states to a map of
;;   input symbols to state/state collection*.
;;   Initial state: A state or collection of states.
;;   Configuration: Collection of states & input sequence.
;;
;; An epsilon transition takes nil as input, but is otherwise like every other
;; transition.
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
        (config
         (apply hash-set
                (apply concat (for [s (epsilon-closure (:transitions this)
                                                       state)]
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
