(in-ns 'game.core)

(def card-definitions-upgrades-oaktown-grid
  {"Oaktown Grid"
   {:events {:pre-trash {:req (req (in-same-server? card target))
                         :effect (effect (trash-cost-bonus 3))}}}})
