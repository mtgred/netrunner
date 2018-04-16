(in-ns 'game.core)

(def card-hardware-desperado
  {"Desperado"
   {:in-play [:memory 1]
    :events {:successful-run {:silent (req true)
                              :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}})
