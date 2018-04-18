(in-ns 'game.core)

(def card-definitions-assets-brain-taping-warehouse
  {"Brain-Taping Warehouse"
   {:events {:pre-rez
             {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
              :effect (effect (rez-cost-bonus (- (:click runner))))}}}})
