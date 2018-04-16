(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-napd-contract
  {"NAPD Contract"
   {:steal-cost-bonus (req [:credit 4])
    :advancement-cost-bonus (req (+ (:bad-publicity corp)
                                    (:has-bad-pub corp)))}})
