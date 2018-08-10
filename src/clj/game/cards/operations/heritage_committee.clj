(in-ns 'game.cards.operations)

(def card-definition-heritage-committee
  {"Heritage Committee"
   {:async true
    :effect (req (wait-for (draw state side 3 nil)
                           (continue-ability state side
                                             {:prompt "Select a card in HQ to put on top of R&D"
                                              :choices {:req #(and (= (:side %) "Corp")
                                                                   (in-hand? %))}
                                              :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                                              :effect (effect (move target :deck {:front true}))}
                                             card nil)))}})
