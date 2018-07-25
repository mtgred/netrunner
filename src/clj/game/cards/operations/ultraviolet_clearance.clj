(in-ns 'game.cards.operations)

(def card-definition-ultraviolet-clearance
  {"Ultraviolet Clearance"
   {:async true
    :effect (req (gain-credits state side 10)
                 (wait-for (draw state side 4 nil)
                           (continue-ability state side
                                             {:prompt "Choose a card in HQ to install"
                                              :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (not (is-type? % "Operation")))}
                                              :msg "gain 10 [Credits], draw 4 cards, and install 1 card from HQ"
                                              :cancel-effect (req (effect-completed state side eid))
                                              :effect (effect (corp-install target nil))}
                                             card nil)))}})
