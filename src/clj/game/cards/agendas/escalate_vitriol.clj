(in-ns 'game.cards.agendas)

(def card-definition-escalate-vitriol
  {"Escalate Vitriol"
   {:abilities [{:label "Gain 1 [Credit] for each Runner tag"
                 :cost [:click 1]
                 :once :per-turn
                 :msg (msg "gain " (:tag runner) " [Credits]")
                 :effect (effect (gain-credits (:tag runner)))}]}})
