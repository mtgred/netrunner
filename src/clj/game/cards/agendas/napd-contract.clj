(in-ns 'game.core)

(def card-definitions-agendas-napd-contract
  {"NAPD Contract"
   {:steal-cost-bonus (req [:credit 4])
    :advancement-cost-bonus (req (+ (:bad-publicity corp)
                                    (:has-bad-pub corp)))}})
