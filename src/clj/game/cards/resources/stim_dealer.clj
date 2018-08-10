(in-ns 'game.cards.resources)

(def card-definition-stim-dealer
  {"Stim Dealer"
   {:events {:runner-turn-begins
             {:effect (req (if (>= (get-counters card :power) 2)
                             (do (add-counter state side card :power (- (get-counters card :power)))
                                 (damage state side eid :brain 1 {:unpreventable true :card card})
                                 (system-msg state side "takes 1 brain damage from Stim Dealer"))
                             (do (add-counter state side card :power 1)
                                 (gain state side :click 1)
                                 (system-msg state side "uses Stim Dealer to gain [Click]"))))}}}})
