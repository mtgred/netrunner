(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-escalate-vitriol
  {"Escalate Vitriol"
   {:abilities [{:label "Gain 1 [Credit] for each Runner tag"
                 :cost [:click 1]
                 :once :per-turn
                 :msg (msg "gain " (:tag runner) " [Credits]")
                 :effect (effect (gain :credit (:tag runner)))}]}})
