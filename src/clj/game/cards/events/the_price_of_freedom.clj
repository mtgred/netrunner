(in-ns 'game.cards.events)

(def card-definition-the-price-of-freedom
  {"The Price of Freedom"
   {:additional-cost [:connection 1]
    :msg "prevent the Corp from advancing cards during their next turn"
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:rfg)))
                    (move (first (:play-area runner)) :rfg))
    :events {:corp-turn-begins
             {:effect (effect (register-turn-flag! card :can-advance
                                (fn [state side card]
                                  ((constantly false)
                                   (toast state :corp "Cannot advance cards this turn due to The Price of Freedom." "warning"))))
                              (unregister-events card))}}}})
