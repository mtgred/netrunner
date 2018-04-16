(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-akitaro-watanabe
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (ice? target)
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}})
