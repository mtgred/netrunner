(in-ns 'game.core)

(def card-definitions-upgrades-rutherford-grid
  {"Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}})
