(in-ns 'game.core)

(def card-definitions-resources-data-folding
  {"Data Folding"
   (let [ability {:label "Gain 1 [Credits] (start of turn)"
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :req (req (and (>= (:memory runner) 2) (:runner-phase-12 @state)))
                  :effect (effect (gain :credit 1))}]
    {:flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})})
