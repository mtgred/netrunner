(in-ns 'game.core)

(def card-hardware-zamba
  {"Zamba"
   {:implementation "Credit gain is automatic"
    :in-play [:memory 2]
    :events {:expose {:effect (effect (gain :runner :credit 1))
                      :msg "gain 1 [Credits]"}}}})
