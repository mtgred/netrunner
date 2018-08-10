(in-ns 'game.cards.upgrades)

(def card-definition-research-station
  {"Research Station"
   {:init {:root "HQ"}
    :install-req (req (filter #{"HQ"} targets))
    :in-play [:hand-size 2]}})
