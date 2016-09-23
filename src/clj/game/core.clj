(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid make-label to-keyword capitalize
                                costs-to-symbol vdissoc distinct-by abs string->num safe-split
                                dissoc-in cancellable card-is? side-str build-cost-str build-spend-msg cost-names
                                zones->sorted-names remote->name remote-num->name central->name zone->name central->zone
                                is-remote? is-central? get-server-type other-side same-side?
                                combine-subtypes remove-subtypes]]
            [game.macros :refer [effect req msg when-completed final-effect continue-ability]]
            [clojure.string :refer [split-lines split join lower-case]]
            [clojure.core.match :refer [match]]))

(declare get-card get-zones get-runnable-zones get-remote-names make-eid make-result register-effect-completed
         get-nested-host resolve-ability say server-card system-msg trigger-event update!)

(def game-states (atom {}))
(def old-states (atom {}))
(def all-cards (atom {}))
(def all-cards-alt (atom {}))

(load "core/core-cards")     ; retrieving and updating cards
(load "core/core-events")    ; triggering of events
(load "core/core-costs")     ; application of costs to play
(load "core/core-rules")     ; core game rules
(load "core/core-turns")     ; the turn sequence
(load "core/core-actions")   ; functions linked to UI actions
(load "core/core-abilities") ; support for card abilities and prompts
(load "core/core-installing"); installing and interacting with installed cards and servers
(load "core/core-hosting")   ; hosting routines
(load "core/core-runs")      ; the run sequence
(load "core/core-ice")       ; ice and icebreaker interactions
(load "core/core-flags")     ; various miscellaneous manipulations of specific effects
(load "core/core-io")        ; routines for parsing input or printing to the log
(load "core/core-misc")      ; misc stuff
(load "cards")               ; card definitions
