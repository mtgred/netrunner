(in-ns 'game.core)

(def card-operations-stock-buy-back
  {"Stock Buy-Back"
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :effect (effect (gain :credit (* 3 (count (:scored runner)))))}})
