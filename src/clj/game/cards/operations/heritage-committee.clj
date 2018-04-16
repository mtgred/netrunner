(in-ns 'game.core)

(def card-operations-heritage-committee
  {"Heritage Committee"
   {:delayed-completion true
    :effect (req (when-completed (draw state side 3 nil)
                                 (continue-ability state side
                                   {:prompt "Select a card in HQ to put on top of R&D"
                                    :choices {:req #(and (= (:side %) "Corp")
                                                         (in-hand? %))}
                                    :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                                    :effect (effect (move target :deck {:front true}))}
                                   card nil)))}})
