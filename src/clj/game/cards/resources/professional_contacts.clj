(in-ns 'game.cards.resources)

(def card-definition-professional-contacts
  {"Professional Contacts"
   {:abilities [{:cost [:click 1]
                 :msg "gain 1 [Credits] and draw 1 card"
                 :effect (effect (gain-credits 1)
                                 (draw))}]}})
