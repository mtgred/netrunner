(ns game.core.process-actions
  (:require
   [clojure.string :as str]
   [game.core.actions :refer [click-advance click-credit click-draw click-run
                              close-deck do-purge flashback generate-install-list
                              generate-runnable-zones move-card expend-ability
                              play play-ability play-corp-ability
                              play-dynamic-ability play-runner-ability play-subroutine play-unbroken-subroutines remove-tag
                              resolve-prompt score select trash-button trash-resource view-deck]]
   [game.core.card :refer [get-card]]
   [game.core.change-vals :refer [change]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.commands :refer [parse-command]]
   [game.core.eid :refer [make-eid]]
   [game.core.moving :refer [trash]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [check-for-empty-server continue handle-end-run
                           jack-out start-next-phase toggle-auto-no-action]]
   [game.core.say :refer [indicate-action say system-msg system-say]]
   [game.core.set-up :refer [keep-hand mulligan]]
   [game.core.shuffling :refer [shuffle-deck]]
   [game.core.toasts :refer [ack-toast]]
   [game.core.turns :refer [end-phase-12 phase-12-pass-priority end-turn end-turn-continue post-discard-pass-priority start-turn]]
   [game.core.winning :refer [concede]]))

(defn checkpoint+clean-up
  [state]
  (fake-checkpoint state)
  ;; End the run if running an empty remote
  (when (or (check-for-empty-server state)
            (:ended (:end-run @state)))
    (handle-end-run state :corp nil)
    (fake-checkpoint state)))

(defn set-property
  "set properties of the game state that need to be adjustable by the frontend
  ie: * do we want an offer to trash like cards on installs?"
  [state side {:keys [key value]}]
  (let [acceptable-keys #{:trash-like-cards :auto-purge
                          :force-phase-12-self :force-phase-12-opponent
                          :force-post-discard-self :force-post-discard-opponent}]
    (cond
      (acceptable-keys key) (swap! state assoc-in [side :properties key] value)
      ;; will throw error otherwise
      )))

(defn should-process-command?
  [state side command]
  (let [prompt-type (get-in @state [side :prompt-state :prompt-type])]
    ;; we can process these commands whenever, they are for fixing stuff
    (or (contains? #{"/close-prompt" "/undo-click" "/undo-turn" "/undo-paid-ability" "/swap-sides" "/save-replay"} (str/trim command))
        ;; but otherwise, we should not process commands unless there is no prompt,
        ;; or just a run prompt
        (not prompt-type)
        (= :run prompt-type))))

(defn command-parser
  [state side {:keys [user text] :as args}]
  (let [author (or user (get-in @state [side :user]))
        text (if (= (str/trim text) "null") " null" text)]
    (if-let [command (parse-command state text)]
      (when (and (not= side nil) (not= side :spectator) (should-process-command? state side text))
        (command state side)
        (system-say state side (str "[!]" (:username author) " uses a command: " text)))
      (say state side args))))

(def commands
  {"ability" #'play-ability
   "advance" #'click-advance
   "change" #'change
   "choice" #'resolve-prompt
   "close-deck" #'close-deck
   "concede" #'concede
   "continue" #'continue
   "corp-ability" #'play-corp-ability
   "credit" #'click-credit
   "derez" #(derez %1 %2 (make-eid %1) (:card %3) {:no-event true})
   "draw" #'click-draw
   "dynamic-ability" #'play-dynamic-ability
   "end-phase-12" #'end-phase-12
   "phase-12-pass-priority" #'phase-12-pass-priority
   "start-next-phase" #'start-next-phase
   "end-turn" #'end-turn
   "post-discard-pass-priority" #'post-discard-pass-priority
   "end-post-discard" #'end-turn-continue
   "flashback" #'flashback
   "generate-install-list" #'generate-install-list
   "generate-runnable-zones" #'generate-runnable-zones
   "indicate-action" #'indicate-action
   "jack-out" #'jack-out
   "keep" #'keep-hand
   "move" #'move-card
   "mulligan" #'mulligan
   "play" #'play
   "expend" #'expend-ability
   "purge" #'do-purge
   "remove-tag" #'remove-tag
   "rez" #(rez %1 %2 (make-eid %1) (:card %3) (dissoc %3 :card))
   "run" #'click-run
   "runner-ability" #'play-runner-ability
   "score" #(score %1 %2 (make-eid %1) (get-card %1 (:card %3)) nil)
   "select" #'select
   "set-property" #'set-property
   "shuffle" #'shuffle-deck
   "start-turn" #'start-turn
   "subroutine" #'play-subroutine
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "toast" #'ack-toast
   "toggle-auto-no-action" #'toggle-auto-no-action
   "trash" #(trash-button %1 %2 (make-eid %1) (get-card %1 (:card %3)))
   "trash-resource" #'trash-resource
   "unbroken-subroutines" #'play-unbroken-subroutines
   "view-deck" #'view-deck})

(defn process-action
  [command state side args]
  (when-let [c (get commands command)]
    (c state side args)
    (checkpoint+clean-up state)
    true))
