(in-ns 'game.cards.agendas)

(def card-definition-domestic-sleepers
  {"Domestic Sleepers"
   {:agendapoints-runner (req 0)
    :abilities [{:cost [:click 3] :msg "place 1 agenda counter on Domestic Sleepers"
                 :req (req (not (:counter card)))
                 :effect (effect (gain-agenda-point 1)
                                 (set-prop card :counter {:agenda 1} :agendapoints 1))}]}})
