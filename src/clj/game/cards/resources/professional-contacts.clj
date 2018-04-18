(in-ns 'game.core)

(def card-definitions-resources-professional-contacts
  {"Professional Contacts"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 1) (draw))
                 :msg "gain 1 [Credits] and draw 1 card"}]}})
