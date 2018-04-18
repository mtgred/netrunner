(in-ns 'game.core)

(def card-definitions-upgrades-experiential-data
  {"Experiential Data"
   {:effect (req (update-ice-in-server state side (card->server state card)))
    :events {:pre-ice-strength {:req (req (protecting-same-server? card target))
                                :effect (effect (ice-strength-bonus 1 target))}}
    :derez-effect {:effect (req (update-ice-in-server state side (card->server state card)))}
    :trash-effect {:effect (req (update-all-ice state side))}}})
