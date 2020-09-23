(in-ns 'game.core)

(defn clear-empty-remotes
  [state]
  (doseq [remote (get-remotes state)]
    (let [zone [:corp :servers (first remote)]]
      (when (and (empty? (get-in @state (conj zone :content)))
                 (empty? (get-in @state (conj zone :ices))))
        (swap! state dissoc-in zone)))))

(defn fake-checkpoint [state]
  ;; Update strength and labels
  (update-all-icebreakers state :runner)
  (update-all-ice state :corp)
  (update-all-card-labels state)
  ;; Clear empty remotes
  (clear-empty-remotes state)
  ;; End the run if running an empty remote
  (when (check-for-empty-server state)
    (handle-end-run state :corp)))

(def commands
  {"ability" play-ability
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
   "rez" #(rez %1 %2 (:card %3) (dissoc %3 :card))
   "run" click-run
   "runner-ability" play-runner-ability
   "score" #(score %1 %2 (get-card %1 (:card %3)))
   "select" select
   "shuffle" shuffle-deck
   "start-turn" start-turn
   "subroutine" play-subroutine
   "system-msg" #(system-msg %1 %2 (:msg %3))
   "toast"  toast
   "toggle-auto-no-action" toggle-auto-no-action
   "trash" #(trash %1 %2 (make-eid %1) (get-card %1 (:card %3)) nil)
   "trash-resource" trash-resource
   "unbroken-subroutines" play-unbroken-subroutines
   "view-deck" view-deck})

(defn process-action
  [command state side args]
  (when-let [c (get commands command)]
    (c state side args)
    (fake-checkpoint state)
    true))
