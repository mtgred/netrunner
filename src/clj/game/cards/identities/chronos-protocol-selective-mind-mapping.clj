(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-chronos-protocol-selective-mind-mapping
  {"Chronos Protocol: Selective Mind-mapping"
   {:events
    {:corp-phase-12 {:effect (effect (enable-corp-damage-choice))}
     :runner-phase-12 {:effect (effect (enable-corp-damage-choice))}
     :pre-resolve-damage
     {:delayed-completion true
      :req (req (and (= target :net)
                     (corp-can-choose-damage? state)
                     (> (last targets) 0)
                     (empty? (filter #(= :net (first %)) (turn-events state :runner :damage)))))
      :effect (req (damage-defer state side :net (last targets))
                   (if (= 0 (count (:hand runner)))
                     (do (swap! state update-in [:damage] dissoc :damage-choose-corp)
                         (damage state side eid :net (get-defer-damage state side :net nil)
                                 {:unpreventable true :card card}))
                     (do (show-wait-prompt state :runner "Corp to use Chronos Protocol: Selective Mind-mapping")
                         (continue-ability
                           state side
                           {:optional
                            {:prompt (str "Use Chronos Protocol: Selective Mind-mapping to reveal the Runner's "
                                          "Grip to select the first card trashed?")
                             :priority 10
                             :player :corp
                             :yes-ability {:prompt (msg "Select a card to trash")
                                           :choices (req (:hand runner)) :not-distinct true
                                           :priority 10
                                           :msg (msg "trash " (:title target)
                                                     (when (pos? (dec (or (get-defer-damage state side :net nil) 0)))
                                                       (str " and deal " (- (get-defer-damage state side :net nil) 1)
                                                            " more net damage")))
                                           :effect (req (clear-wait-prompt state :runner)
                                                        (swap! state update-in [:damage] dissoc :damage-choose-corp)
                                                        (trash state side target {:cause :net :unpreventable true})
                                                        (let [more (dec (or (get-defer-damage state side :net nil) 0))]
                                                          (damage-defer state side :net more {:part-resolved true})))}
                             :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                       (swap! state update-in [:damage] dissoc :damage-choose-corp))}}}
                           card nil))))}}
    :req (req (empty? (filter #(= :net (first %)) (turn-events state :runner :damage))))
    :effect (effect (enable-corp-damage-choice))
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-corp))}})
