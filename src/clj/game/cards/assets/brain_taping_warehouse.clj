(in-ns 'game.cards.assets)

(def card-definition-brain-taping-warehouse
  {"Brain-Taping Warehouse"
   {:events {:pre-rez
             {:req (req (and (ice? target)
                             (has-subtype? target "Bioroid")))
              :effect (effect (rez-cost-bonus (- (:click runner))))}}}})
