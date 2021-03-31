(ns game.core.process-actions
  (:require
    [game.core.actions :refer [click-advance click-credit click-draw click-run close-deck do-purge generate-install-list generate-runnable-zones move-card play play-ability play-corp-ability play-dynamic-ability play-runner-ability play-subroutine play-unbroken-subroutines remove-tag resolve-prompt score select trash-resource view-deck]]
    [game.core.board :refer [get-remotes]]
    [game.core.card :refer [get-card]]
    [game.core.change-vals :refer [change]]
    [game.core.checkpoint :refer [fake-checkpoint]]
    [game.core.eid :refer [make-eid]]
    [game.core.moving :refer [trash]]
    [game.core.rezzing :refer [derez rez]]
    [game.core.runs :refer [check-for-empty-server continue corp-phase-43 handle-end-run jack-out start-next-phase toggle-auto-no-action]]
    [game.core.say :refer [indicate-action system-msg]]
    [game.core.set-up :refer [keep-hand mulligan]]
    [game.core.shuffling :refer [shuffle-deck]]
    [game.core.toasts :refer [toast]]
    [game.core.turns :refer [end-phase-12 end-turn start-turn]]
    [game.core.winning :refer [concede]]
    [game.utils :refer [dissoc-in]]
    [clojure.string :as string]))

(defn checkpoint+clean-up
  [state]
  (fake-checkpoint state)
  ;; End the run if running an empty remote
  (when (or (check-for-empty-server state)
            (:ended (:run @state)))
    (handle-end-run state :corp)
    (fake-checkpoint state)))

(defn commands
  [command]
  (case command
    "ability" play-ability
    "advance" click-advance
    "change" change
    "choice" resolve-prompt
    "close-deck" close-deck
    "concede" concede
    "continue" continue
    "corp-ability" play-corp-ability
    "corp-phase-43" corp-phase-43
    "credit" click-credit
    "derez" #(derez %1 %2 (:card %3))
    "draw" click-draw
    "dynamic-ability" play-dynamic-ability
    "end-phase-12" end-phase-12
    "start-next-phase" start-next-phase
    "end-turn" end-turn
    "generate-install-list" generate-install-list
    "generate-runnable-zones" generate-runnable-zones
    "indicate-action" indicate-action
    "jack-out" jack-out
    "keep" keep-hand
    "move" move-card
    "mulligan" mulligan
    "play" play
    "purge" do-purge
    "remove-tag" remove-tag
    "rez" #(rez %1 %2 (make-eid %1) (:card %3) (dissoc %3 :card))
    "run" click-run
    "runner-ability" play-runner-ability
    "score" #(score %1 %2 (make-eid %1) (get-card %1 (:card %3)) nil)
    "select" select
    "shuffle" shuffle-deck
    "start-turn" start-turn
    "subroutine" play-subroutine
    "system-msg" #(system-msg %1 %2 (:msg %3))
    "toast" toast
    "toggle-auto-no-action" toggle-auto-no-action
    "trash" #(trash %1 %2 (make-eid %1) (get-card %1 (:card %3)) (dissoc %3 :card))
    "trash-resource" trash-resource
    "unbroken-subroutines" play-unbroken-subroutines
    "view-deck" view-deck))

(defn process-action
  [command state side args]
  (when-let [c (commands command)]
    (c state side args)
    (checkpoint+clean-up state)
    true))
