(ns game.core
  (:require [game.core.eid :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [card-def] :as card-defs]
            [game.core.prompts :refer :all]
            [game.core.toasts :refer [toast show-error-toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [game.core.state :refer :all]
            [game.core.player :refer :all]
            [game.core.effects :refer :all]
            [clj-time.core :as t]
            [clj-uuid :as uuid]
            [clojure.string :as string :refer [split-lines split join lower-case includes? starts-with? blank?]]
            [clojure.java.io :as io]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.edn :as edn]
            [clojure.set :as clj-set]
            [jinteki.utils :refer :all]
            [jinteki.cards :refer [all-cards]]
            [tasks.nrdb :refer [replace-collection update-config]]
            [tasks.altart :refer [add-art]]
            [game.quotes :as quotes])
  (:import [game.core.state State]
           [game.core.player Corp Runner]
           [game.core.card Card]))

(load "core/to_string")    ; "toString" functions
(load "core/board")        ; Helpers for retrieving installed cards
(load "core/flags")        ; various miscellaneous manipulations of specific effects
(load "core/events")       ; triggering of events
(load "core/gaining")      ; gain/lose credits and clicks and other base values
(load "core/cards")        ; retrieving and updating cards

;; Cost section
(load "core/costs/cost_impls") ; implementations of cost functions
(load "core/costs/payment") ; payment related functions
(load "core/costs/cost_labels") ; cost-aware ability label generation and application
(load "core/costs/cost_generation") ; state-aware cost-generating functions

(load "core/io")           ; routines for parsing input or printing to the log
(load "core/ice")          ; ice and icebreaker interactions
(load "core/rules")        ; core game rules
(load "core/trashing")     ; trashing cards
(load "core/turns")        ; the turn sequence

;; Abilities
(load "core/resolve_ability") ; support for card abilities and prompts
(load "core/optional")
(load "core/psi")
(load "core/traces")

(load "core/initializing") ; initializing cards
(load "core/hosting")      ; hosting routines
(load "core/installing")   ; installing and interacting with installed cards and servers
(load "core/access")       ; accessing rules
(load "core/runs")         ; the run sequence
(load "core/commands")     ; chat commands
(load "core/misc")         ; misc stuff
(load "core/actions")      ; functions linked to UI actions
(load "core/def_helpers")  ; card definitions
(load "core/process_actions") ; things that need to be run after every action
