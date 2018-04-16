(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-rutherford-grid
  {"Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}})