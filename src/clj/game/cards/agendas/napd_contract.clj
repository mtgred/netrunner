(in-ns 'game.cards.agendas)

(def card-definition-napd-contract
  {"NAPD Contract"
   {:steal-cost-bonus (req [:credit 4])
    :advancement-cost-bonus (req (+ (:bad-publicity corp)
                                    (:has-bad-pub corp)))}})
