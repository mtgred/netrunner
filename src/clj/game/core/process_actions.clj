(ns game.core.process-actions
  (:require
   [clojure.string :as str]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [game.core.actions :refer [click-advance click-credit click-draw click-run
                              close-deck do-purge generate-install-list
                              generate-runnable-zones move-card expend-ability
                              play play-ability play-corp-ability
                              play-dynamic-ability play-runner-ability play-subroutine play-unbroken-subroutines remove-tag
                              resolve-prompt score select trash-resource view-deck]]
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
   [game.core.turns :refer [end-phase-12 end-turn start-turn]]
   [game.core.winning :refer [concede]]))

(defn checkpoint+clean-up
  [state]
  (fake-checkpoint state)
  ;; End the run if running an empty remote
  (when (or (check-for-empty-server state)
            (:ended (:end-run @state)))
    (handle-end-run state :corp nil)
    (fake-checkpoint state)))

(defn command-parser
  [state side {:keys [user text] :as args}]
  (let [author (or user (get-in @state [side :user]))
        text (if (= (str/trim text) "null") " null" text)]
    (if-let [command (parse-command text)]
      (when (and (not= side nil) (not= side :spectator))
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
   "derez" #(derez %1 %2 (:card %3))
   "draw" #'click-draw
   "dynamic-ability" #'play-dynamic-ability
   "end-phase-12" #'end-phase-12
   "start-next-phase" #'start-next-phase
   "end-turn" #'end-turn
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
   "shuffle" #'shuffle-deck
   "start-turn" #'start-turn
   "subroutine" #'play-subroutine
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "toast" #'ack-toast
   "toggle-auto-no-action" #'toggle-auto-no-action
   "trash" #(trash %1 %2 (make-eid %1) (get-card %1 (:card %3)) (dissoc %3 :card))
   "trash-resource" #'trash-resource
   "unbroken-subroutines" #'play-unbroken-subroutines
   "view-deck" #'view-deck})

(def unmonitored-commands
  ["generate-install-list"
   "generate-runnable-zones"
   "system-msg"
   "toast"])

(def command-tolerance-ms 30)

(defn process-monitored-command?
  [command state side]
  (let [is-unmonitored? (some #(= % command) unmonitored-commands)
        last-cmd-time (get-in @state [side :last-processed-action])
        current-time (inst/now)
        diff-millis (if last-cmd-time
                      (- (.toEpochMilli current-time) (.toEpochMilli last-cmd-time))
                      (inc command-tolerance-ms))]
    (if is-unmonitored?
      true
      (if (> (abs diff-millis) command-tolerance-ms)
        (do (swap! state assoc-in [side :last-processed-action] current-time)
            true)
        nil))))

(defn process-action
  [command state side args]
  (when-let [c (get commands command)]
    (when (process-monitored-command? command state side)
      (c state side args)
      (checkpoint+clean-up state))
    true))
