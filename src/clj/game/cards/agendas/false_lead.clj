(in-ns 'game.cards.agendas)

(def card-definition-false-lead
  {"False Lead"
   {:abilities [{:req (req (>= (:click runner) 2))
                 :msg "force the Runner to lose [Click][Click]"
                 :effect (effect (forfeit card)
                                 (lose :runner :click 2))}]}})
