(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-hokusai-grid
  {"Hokusai Grid"
   {:events {:successful-run {:req (req this-server) :msg "do 1 net damage"
                              :delayed-completion true
                              :effect (effect (damage eid :net 1 {:card card}))}}}})