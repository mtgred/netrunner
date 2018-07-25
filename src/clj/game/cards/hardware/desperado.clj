(in-ns 'game.cards.hardware)

(def card-definition-desperado
  {"Desperado"
   {:in-play [:memory 1]
    :events {:successful-run {:silent (req true)
                              :msg "gain 1 [Credits]" :effect (effect (gain-credits 1))}}}})
