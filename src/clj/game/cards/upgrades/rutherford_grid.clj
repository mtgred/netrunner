(in-ns 'game.cards.upgrades)

(def card-definition-rutherford-grid
  {"Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}})
