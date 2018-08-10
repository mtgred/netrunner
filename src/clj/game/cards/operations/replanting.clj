(in-ns 'game.cards.operations)

(def card-definition-replanting
  {"Replanting"
   (letfn [(replant [n]
             {:prompt "Select a card to install with Replanting"
              :async true
              :choices {:req #(and (= (:side %) "Corp")
                                   (not (is-type? % "Operation"))
                                   (in-hand? %))}
              :effect (req (wait-for (corp-install state side target nil {:no-install-cost true})
                                     (if (< n 2)
                                       (continue-ability state side (replant (inc n)) card nil)
                                       (effect-completed state side eid))))})]
     {:async true
      :prompt "Select an installed card to add to HQ"
      :choices {:req #(and (= (:side %) "Corp")
                           (installed? %))}
      :msg (msg "add " (card-str state target) " to HQ, then install 2 cards ignoring all costs")
      :effect (req (move state side target :hand)
                   (resolve-ability state side (replant 1) card nil))})})
