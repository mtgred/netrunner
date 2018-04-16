(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-research-station
  {"Research Station"
   {:init {:root "HQ"}
    :in-play [:hand-size-modification 2]}})