(in-ns 'game.core)

(def card-definitions-operations-ultraviolet-clearance
  {"Ultraviolet Clearance"
   {:delayed-completion true
    :effect (req (gain state side :credit 10)
                 (when-completed (draw state side 4 nil)
                                 (continue-ability state side
                                                   {:prompt "Choose a card in HQ to install"
                                                    :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (not (is-type? % "Operation")))}
                                                    :msg "gain 10 [Credits], draw 4 cards, and install 1 card from HQ"
                                                    :cancel-effect (req (effect-completed state side eid))
                                                    :effect (effect (corp-install target nil))}
                                                   card nil)))}})
