(ns game.cards.events
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [jinteki.utils :refer [str->int other-side]]
            [jinteki.cards :refer [all-cards]]))

(defn run-event
  ([] (run-event nil))
  ([run-ability] (run-event nil run-ability))
  ([cdef run-ability] (run-event cdef run-ability nil))
  ([cdef run-ability pre-run-effect]
   (run-event cdef run-ability pre-run-effect nil))
  ([cdef run-ability pre-run-effect post-run-effect]
   (merge {:prompt "Choose a server"
           :choices (req runnable-servers)
           :effect (effect ((or pre-run-effect (effect)) eid card targets)
                           (run target run-ability card)
                           ((or post-run-effect (effect)) eid card targets))}
          cdef)))

(defn cutlery
  [subtype]
  ;; Subtype does nothing currently, but might be used if trashing is properly implemented
  {:implementation "Ice trash is manual, always enables Reprisals"
   :prompt "Choose a server"
   :choices (req runnable-servers)
   :effect (req (run state :runner target nil card)
                (swap! state assoc-in [:runner :register :trashed-card] true))})
