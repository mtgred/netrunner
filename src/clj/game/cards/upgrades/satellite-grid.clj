(in-ns 'game.core)

(def card-definitions-upgrades-satellite-grid
  {"Satellite Grid"
   {:effect (req (doseq [c (:ices (card->server state card))]
                   (set-prop state side c :extra-advance-counter 1))
                 (update-all-ice state side))
    :events {:corp-install {:req (req (and (ice? target)
                                           (protecting-same-server? card target)))
                            :effect (effect (set-prop target :extra-advance-counter 1))}}
    :leave-play (req (doseq [c (:ices (card->server state card))]
                       (update! state side (dissoc c :extra-advance-counter)))
                     (update-all-ice state side))}})
