(in-ns 'game.cards.assets)

(def card-definition-private-contracts
  {"Private Contracts"
   {:effect (effect (add-counter card :credit 14))
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 2]
                 :msg "gain 2 [Credits]"
                 :effect (req (take-credits state :corp 2)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (trash state :corp card)))}]}})
