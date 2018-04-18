(in-ns 'game.core)

(def card-definitions-resources-stim-dealer
  {"Stim Dealer"
   {:events {:runner-turn-begins
             {:effect (req (if (>= (get-in card [:counter :power] 0) 2)
                             (do (add-counter state side card :power (- (get-in card [:counter :power] 0)))
                                 (damage state side eid :brain 1 {:unpreventable true :card card})
                                 (system-msg state side "takes 1 brain damage from Stim Dealer"))
                             (do (add-counter state side card :power 1)
                                 (gain state side :click 1)
                                 (system-msg state side "uses Stim Dealer to gain [Click]"))))}}}})
