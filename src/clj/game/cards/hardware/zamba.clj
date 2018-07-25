(in-ns 'game.cards.hardware)

(def card-definition-zamba
  {"Zamba"
   {:implementation "Credit gain is automatic"
    :in-play [:memory 2]
    :events {:expose {:effect (effect (gain-credits :runner 1))
                      :msg "gain 1 [Credits]"}}}})
