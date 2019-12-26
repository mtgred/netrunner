(ns game.cards.basic
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [define-card]]
            [game.core.eid :refer [make-eid make-result effect-completed]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability when-let*]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

;; Card definitions

(define-card "Corp Basic Action Card"
  {:abilities [{:label "Gain 1 [Credits]"
                :cost [:click]
                :msg "gain 1 [Credits]"
                :effect (req (gain-credits state side 1 :corp-click-credit)
                             (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                             (trigger-event state side :corp-click-credit)
                             (play-sfx state side "click-credit"))}
               {:label "Draw 1 card"
                :cost [:click]
                :msg "draw 1 card"
                :async true
                :effect (req (wait-for (trigger-event-simult state side (make-eid state eid) :pre-corp-click-draw nil nil)
                                       (trigger-event state side :corp-click-draw (->> @state side :deck (take 1)))
                                       (draw state side)
                                       (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                                       (play-sfx state side "click-card")))}
               {:label "Install 1 agenda, asset, upgrade, or piece of ice from HQ"
                :async true
                :req (req (not-empty (:hand corp)))
                :effect (req (let [target-card (first targets)
                                   server (second targets)]
                               (corp-install state side (make-eid state {:source server :source-type :corp-install})
                                             target-card server {:base-cost [:click 1] :action :corp-click-install})))}
               {:label "Play 1 operation"
                :async true
                :req (req (not-empty (:hand corp)))
                :effect (req (let [target-card (first targets)]
                               (play-instant state side (make-eid state {:source :action :source-type :play})
                                             target-card {:base-cost [:click 1]})))}
               ]})

(define-card "Runner Basic Action Card"
  {:abilities [{:label "Gain 1 [Credits]"
                :cost [:click]
                :msg "gain 1 [Credits]"
                :effect (req (gain-credits state side 1 :runner-click-credit)
                             (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                             (trigger-event state side :runner-click-credit)
                             (play-sfx state side "click-credit"))}
               {:label "Draw 1 card"
                :cost [:click]
                :msg "draw 1 card"
                :effect (req (wait-for (trigger-event-simult state side (make-eid state eid) :pre-runner-click-draw nil nil)
                                       (trigger-event state side :runner-click-draw (->> @state side :deck (take 1)))
                                       (draw state side)
                                       (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                                       (play-sfx state side "click-card")))}
               {:label "Install 1 program, resource, or piece of hardware from the grip"
                :async true
                :req (req (not-empty (:hand runner)))
                :effect (req (let [target-card (first targets)]
                               (runner-install state side (make-eid state {:source :action :source-type :runner-install})
                                               target-card {:base-cost [:click 1]})))}
               {:label "Play 1 event"
                :async true
                :req (req (not-empty (:hand runner)))
                :effect (req (let [target-card (first targets)]
                               (play-instant state side (make-eid state {:source :action :source-type :play})
                                             target-card {:base-cost [:click 1]})))}
               ]})
