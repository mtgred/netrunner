(in-ns 'game.cards.hardware)

(def card-definition-titanium-ribs
  {"Titanium Ribs"
   {:events
    {:pre-resolve-damage
     {:async true
      :req (req (and (pos? (last targets))
                     (runner-can-choose-damage? state)
                     (not (get-in @state [:damage :damage-replace]))))
      :effect (req (let [dtype target
                         src (second targets)
                         dmg (last targets)]
                     (when (> dmg (count (:hand runner)))
                       (flatline state))
                     (when (= dtype :brain)
                       (swap! state update-in [:runner :brain-damage] #(+ % dmg))
                       (swap! state update-in [:runner :hand-size :mod] #(- % dmg)))
                     (show-wait-prompt state :corp "Runner to use Titanium Ribs to choose cards to be trashed")
                     (wait-for (resolve-ability
                                 state side
                                 {:async true
                                  :prompt (msg "Select " dmg " cards to trash for the " (name dtype) " damage")
                                  :player :runner
                                  :choices {:max dmg :all true :req #(and (in-hand? %) (= (:side %) "Runner"))}
                                  :msg (msg "trash " (join ", " (map :title targets)))
                                  :effect (req (clear-wait-prompt state :corp)
                                               (doseq [c targets]
                                                 (trash state side c {:cause dtype :unpreventable true}))
                                               (trigger-event state side :damage-chosen)
                                               (damage-defer state side dtype 0)
                                               (effect-completed state side eid))}
                                 card nil)
                               (trigger-event-sync state side eid :damage dtype src dmg))))}
    :damage-chosen {:effect (effect (enable-runner-damage-choice))}}
    :async true
    :effect (effect (enable-runner-damage-choice)
                    (system-msg (str "suffers 2 meat damage from installing Titanium Ribs"))
                    (damage eid :meat 2 {:unboostable true :card card}))
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-runner))}})
