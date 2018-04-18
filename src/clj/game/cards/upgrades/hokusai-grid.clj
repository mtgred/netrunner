(in-ns 'game.core)

(def card-definitions-upgrades-hokusai-grid
  {"Hokusai Grid"
   {:events {:successful-run {:req (req this-server) :msg "do 1 net damage"
                              :delayed-completion true
                              :effect (effect (damage eid :net 1 {:card card}))}}}})
