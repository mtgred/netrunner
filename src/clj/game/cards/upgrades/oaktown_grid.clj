(in-ns 'game.cards.upgrades)

(def card-definition-oaktown-grid
  {"Oaktown Grid"
   {:events {:pre-trash {:req (req (in-same-server? card target))
                         :effect (effect (trash-cost-bonus 3))}}}})
