(in-ns 'game.core)

(def card-definitions-agendas-gila-hands-arcology
  {"Gila Hands Arcology"
   {:abilities [{:cost [:click 2]
                 :msg "gain 3 [Credits]"
                 :effect (effect (gain :credit 3))}]}})
