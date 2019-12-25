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
  {:abilities [{:label "Draw"
                :msg "draw 1 card"
                :effect (req
                          (wait-for (trigger-event-simult state side (make-eid state eid) :pre-corp-click-draw nil nil)
                                    (trigger-event state side :corp-click-draw (->> @state side :deck (take 1)))
                                    (draw state side)
                                    (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                                    (play-sfx state side "click-card")))}]})

(define-card "Runner Basic Action Card"
  {:abilities [{:label "Draw"
                :msg "draw 1 card"
                :effect (req
                          (wait-for (trigger-event-simult state side (make-eid state eid) :pre-corp-click-draw nil nil)
                                    (trigger-event state side :corp-click-draw (->> @state side :deck (take 1)))
                                    (draw state side)
                                    (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                                    (play-sfx state side "click-card")))}]})
