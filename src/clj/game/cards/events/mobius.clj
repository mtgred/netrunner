(in-ns 'game.cards.events)

(def card-definition-mobius
  {"Möbius"
   {:req (req rd-runnable)
    :async true
    :effect (req (let [mob-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] mob-eid)
                                    (assoc card :zone '(:discard)))
                   (wait-for (game.core/run state side mob-eid :rd nil card)
                             (let [card (get-card state (assoc card :zone '(:discard)))]
                               (unregister-events state side card)
                               (when (:run-again card)
                                 (game.core/run state side mob-eid :rd nil card)
                                 (register-events state side {:successful-run
                                                              {:req (req (= target :rd))
                                                               :msg "gain 4 [Credits]"
                                                               :effect (effect (gain-credits 4)
                                                                               (unregister-events card))}}
                                               (assoc card :zone '(:discard))))
                            (update! state side (dissoc card :run-again))))))
    :events {:successful-run nil
             :successful-run-ends {:interactive (req true)
                                   :optional {:req (req (= [:rd] (:server target)))
                                              :prompt "Make another run on R&D?"
                                              :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                            (update! (assoc card :run-again true)))}}}}}})
