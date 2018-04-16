(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-oaktown-grid
  {"Oaktown Grid"
   {:events {:pre-trash {:req (req (in-same-server? card target))
                         :effect (effect (trash-cost-bonus 3))}}}})