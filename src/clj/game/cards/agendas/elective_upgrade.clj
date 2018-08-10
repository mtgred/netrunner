(in-ns 'game.cards.agendas)

(def card-definition-elective-upgrade
  {"Elective Upgrade"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 2))
    :abilities [{:cost [:click 1]
                 :counter-cost [:agenda 1]
                 :once :per-turn
                 :effect (effect (gain :click 2))
                 :msg "gain [Click][Click]"}]}})
