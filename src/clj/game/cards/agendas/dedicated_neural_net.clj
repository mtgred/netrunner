(in-ns 'game.cards.agendas)

(def card-definition-dedicated-neural-net
  {"Dedicated Neural Net"
    (let [psi-effect
           {:async true
            :mandatory true
            :effect (req (if (not-empty (:hand corp))
                           (do (show-wait-prompt state :runner "Corp to select cards in HQ to be accessed")
                               (continue-ability
                                 state :corp
                                 {:prompt (msg "Select " (access-count state side :hq-access) " cards in HQ for the Runner to access")
                                  :choices {:req #(and (in-hand? %) (card-is? % :side :corp))
                                            :max (req (access-count state side :hq-access))}
                                  :effect (effect (clear-wait-prompt :runner)
                                                  (continue-ability :runner
                                                                    (access-helper-hq
                                                                      state (access-count state side :hq-access)
                                                                      ; access-helper-hq uses a set to keep track of which cards have already
                                                                      ; been accessed. Using the set difference we make the runner unable to
                                                                      ; access non-selected cards from the corp prompt
                                                                      (clojure.set/difference (set (:hand corp)) (set targets)))
                                                                    card nil))}
                                 card nil))
                           (effect-completed state side eid)))}]
       {:events {:successful-run {:interactive (req true)
                                  :psi {:req (req (= target :hq))
                                        :once :per-turn
                                        :not-equal {:effect (req (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                                   (swap! state update-in [:run :run-effect]
                                                                          #(assoc % :replace-access psi-effect)))
                                                                 (effect-completed state side eid))}}}}})})
