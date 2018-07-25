(in-ns 'game.cards.operations)

(def card-definition-subliminal-messaging
  {"Subliminal Messaging"
   (letfn [(subliminal []
             {:corp-phase-12
              {:effect
               (req (if (not-last-turn? state :runner :made-run)
                      (do (resolve-ability state side
                                           {:optional
                                            {:prompt "Add Subliminal Messaging to HQ?"
                                             :yes-ability {:effect (req (move state side card :hand)
                                                                        (system-msg state side "adds Subliminal Messaging to HQ"))}
                                             :no-ability {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}}}
                                           card nil)
                          (unregister-events state side card))
                      (do (unregister-events state side card)
                          (resolve-ability state side
                                           {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}
                                           card nil))))}})]
     {:msg "gain 1 [Credits]"
      :effect (effect (gain-credits 1)
                      (resolve-ability {:once :per-turn :once-key :subliminal-messaging
                                        :msg "gain [Click]"
                                        :effect (effect (gain :corp :click 1))} card nil))
      :move-zone (req (if (= [:discard] (:zone card))
                        (register-events state side (subliminal) (assoc card :zone '(:discard)))
                        (unregister-events state side card)))
      :events {:corp-phase-12 nil}})})
