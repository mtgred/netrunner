(in-ns 'game.core)

(def card-definitions-upgrades-akitaro-watanabe
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (ice? target)
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}})
