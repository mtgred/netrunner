(in-ns 'game.core)

(def card-definitions-assets-haas-arcology-ai
  {"Haas Arcology AI"
   {:advanceable :while-unrezzed
    :abilities [{:label "Gain [Click][Click]" :once :per-turn :msg "gain [Click][Click]"
                 :cost [:click 1] :advance-counter-cost 1 :effect (effect (gain :click 2))}]}})
