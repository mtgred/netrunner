(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-chilo-city-grid
  {"ChiLo City Grid"
   {:events {:successful-trace {:req (req this-server)
                                :delayed-completion true
                                :effect (effect (tag-runner :runner eid 1))
                                :msg "give the Runner 1 tag"}}}})