(in-ns 'game.cards.agendas)

(def card-definition-efficiency-committee
  {"Efficiency Committee"
   {:silent (req true)
    :effect (effect (add-counter card :agenda 3))
    :abilities [{:cost [:click 1] :counter-cost [:agenda 1]
                 :effect (effect (gain :click 2)
                                 (register-turn-flag!
                                   card :can-advance
                                   (fn [state side card]
                                     ((constantly false)
                                       (toast state :corp "Cannot advance cards this turn due to Efficiency Committee." "warning")))))
                 :msg "gain [Click][Click]"}]}})
