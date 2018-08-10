(in-ns 'game.cards.agendas)

(def card-definition-gila-hands-arcology
  {"Gila Hands Arcology"
   {:abilities [{:cost [:click 2]
                 :msg "gain 3 [Credits]"
                 :effect (effect (gain-credits 3))}]}})
