(in-ns 'game.cards.resources)

(def card-definition-all-nighter
  {"All-nighter"
   {:abilities [{:cost [:click 1]
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :click 2))
                 :msg "gain [Click][Click]"}]}})
