(ns game.core
  (:require [game.utils :refer [remove-once has? merge-costs zone make-cid to-keyword capitalize
                                costs-to-symbol vdissoc distinct-by abs string->num safe-split
                                dissoc-in cancellable card-is?
                                build-spend-msg cost-names remote->name central->name zone->name central->zone
                                is-remote? is-central? get-server-type other-side]]
            [game.macros :refer [effect req msg]]
            [clojure.string :refer [split-lines split join lower-case]]
            [clojure.core.match :refer [match]]))

(declare get-card resolve-ability say system-msg trigger-event update!)

(def game-states (atom {}))

(load "core-cards")     ; retrieving and updating cards
(load "core-events")    ; triggering of events
(load "core-costs")     ; application of costs to play
(load "core-rules")     ; core game rules
(load "core-turns")     ; the turn sequence
(load "core-actions")   ; functions linked to UI actions
(load "core-abilities") ; support for card abilities and prompts
(load "core-installing"); installing and interacting with installed cards and servers
(load "core-hosting")   ; hosting routines
(load "core-runs")      ; the run sequence
(load "core-ice")       ; ice and icebreaker interactions
(load "core-flags")     ; various miscellaneous manipulations of specific effects
(load "core-io")        ; routines for parsing input or printing to the log
(load "core-misc")      ; misc stuff
(load "cards")          ; card definitions
