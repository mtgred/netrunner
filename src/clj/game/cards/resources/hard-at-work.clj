(in-ns 'game.core)

(def card-definitions-resources-hard-at-work
  {"Hard at Work"
   (let [ability {:msg "gain 2 [Credits] and lose [Click]"
                  :once :per-turn
                  :effect (effect (lose :click 1) (gain :credit 2))}]
   {:flags {:drip-economy true}
    :events {:runner-turn-begins ability}
    :abilities [ability]})})
