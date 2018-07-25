(in-ns 'game.cards.upgrades)

(def card-definition-chilo-city-grid
  {"ChiLo City Grid"
   {:events {:successful-trace {:req (req this-server)
                                :async true
                                :effect (effect (gain-tags :corp eid 1))
                                :msg "give the Runner 1 tag"}}}})
