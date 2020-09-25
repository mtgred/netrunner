(ns game.core
  (:require [game.core.abilities]
            [game.core.board]
            [game.core.card]
            [game.core.card-defs]
            [game.core.cost-fns]
            [game.core.effects]
            [game.core.eid]
            [game.core.events]
            [game.core.finding]
            [game.core.flags]
            [game.core.gaining]
            [game.core.ice]
            [game.core.initializing]
            [game.core.player]
            [game.core.prompts]
            [game.core.props]
            [game.core.say]
            [game.core.state]
            [game.core.to-string]
            [game.core.toasts]
            [game.core.update]
            [game.macros]
            [game.utils :refer :all]
            [game.quotes :as quotes]
            [jinteki.utils :refer :all]
            [jinteki.cards :refer [all-cards]]
            [clj-time.core :as t]
            [clj-uuid :as uuid]
            [clojure.string :as string :refer [split-lines split join lower-case includes? starts-with? blank?]]
            [clojure.java.io :as io]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.edn :as edn]
            [clojure.set :as clj-set]
            [potemkin :refer [import-vars]]
            )
  (:import [game.core.state State]
           [game.core.player Corp Runner]
           [game.core.card Card]))

(load "core_ns/import_namespaces") ; Import and re-export all of the separate core namespaces

(load "core_ns/cards")        ; retrieving and updating cards

;; Cost section
(load "core_ns/cost_impls") ; implementations of cost functions

(load "core_ns/rules")        ; core game rules
(load "core_ns/trashing")     ; trashing cards
(load "core_ns/turns")        ; the turn sequence

;; Abilities
(load "core_ns/optional")
(load "core_ns/psi")
(load "core_ns/traces")

(load "core_ns/initializing") ; initializing cards
(load "core_ns/hosting")      ; hosting routines
(load "core_ns/installing")   ; installing and interacting with installed cards and servers
(load "core_ns/access")       ; accessing rules
(load "core_ns/runs")         ; the run sequence
(load "core_ns/commands")     ; chat commands
(load "core_ns/misc")         ; misc stuff
(load "core_ns/actions")      ; functions linked to UI actions
(load "core_ns/def_helpers")  ; card definitions
(load "core_ns/process_actions") ; things that need to be run after every action
