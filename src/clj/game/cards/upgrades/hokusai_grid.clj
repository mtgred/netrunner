(in-ns 'game.cards.upgrades)

(def card-definition-hokusai-grid
  {"Hokusai Grid"
   {:events {:successful-run {:req (req this-server)
                              :msg "do 1 net damage"
                              :async true
                              :effect (effect (damage eid :net 1 {:card card}))}}}})
