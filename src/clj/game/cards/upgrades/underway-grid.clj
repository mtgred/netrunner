(in-ns 'game.core)

(def card-definitions-upgrades-underway-grid
  {"Underway Grid"
   {:implementation "Bypass prevention is not implemented"
    :events {:pre-expose {:req (req (same-server? card target))
                          :msg "prevent 1 card from being exposed"
                          :effect (effect (expose-prevent 1))}}}})
