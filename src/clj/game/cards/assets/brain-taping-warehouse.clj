(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-brain-taping-warehouse
  {"Brain-Taping Warehouse"
   {:events {:pre-rez
             {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
              :effect (effect (rez-cost-bonus (- (:click runner))))}}}})
