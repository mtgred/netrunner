(in-ns 'game.cards.operations)

(def card-definition-stock-buy-back
  {"Stock Buy-Back"
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :effect (effect (gain-credits (* 3 (count (:scored runner)))))}})
