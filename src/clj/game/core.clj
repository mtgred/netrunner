(ns game.core
  (:require [game.core.eid :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [card-def]]
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
            [clojure.core.match :refer [match]]
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

(load "core/events")       ; triggering of events
(load "core/cards")        ; retrieving and updating cards
(load "core/costs")        ; application of costs to play
(load "core/rules")        ; core game rules
(load "core/trashing")     ; trashing cards
(load "core/turns")        ; the turn sequence
(load "core/actions")      ; functions linked to UI actions
(load "core/abilities")    ; support for card abilities and prompts
(load "core/initializing") ; initializing cards
(load "core/installing")   ; installing and interacting with installed cards and servers
(load "core/hosting")      ; hosting routines
(load "core/runs")         ; the run sequence
(load "core/access")       ; accessing rules
(load "core/ice")          ; ice and icebreaker interactions
(load "core/flags")        ; various miscellaneous manipulations of specific effects
(load "core/io")           ; routines for parsing input or printing to the log
(load "core/misc")         ; misc stuff
(load "cards")             ; card definitions
