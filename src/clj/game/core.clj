(ns game.core
  (:require [game.core.eid :refer [make-eid make-result register-effect-completed effect-completed complete-with-result]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer :all]
            [game.core.toasts :refer [toast show-error-toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clj-time.core :as t]
            [clojure.string :as string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.edn :as edn]
            [jinteki.utils :refer [str->int other-side is-tagged? has-subtype? count-bad-pub]]
            [jinteki.cards :refer [all-cards]]
            [tasks.nrdb :refer [replace-collection update-config]]
            [tasks.altart :refer [add-art]]
            [game.quotes :as quotes]))

(load "core/events")    ; triggering of events
(load "core/cards")     ; retrieving and updating cards
(load "core/costs")     ; application of costs to play
(load "core/rules")     ; core game rules
(load "core/turns")     ; the turn sequence
(load "core/actions")   ; functions linked to UI actions
(load "core/abilities") ; support for card abilities and prompts
(load "core/installing"); installing and interacting with installed cards and servers
(load "core/hosting")   ; hosting routines
(load "core/runs")      ; the run sequence
(load "core/ice")       ; ice and icebreaker interactions
(load "core/flags")     ; various miscellaneous manipulations of specific effects
(load "core/io")        ; routines for parsing input or printing to the log
(load "core/misc")      ; misc stuff
(load "cards")          ; card definitions
