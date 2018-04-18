(in-ns 'game.core)

(def card-definitions-assets-thomas-haas
  {"Thomas Haas"
   {:advanceable :always
    :abilities [{:label "Gain credits" :msg (msg "gain " (* 2 (get card :advance-counter 0)) " [Credits]")
                 :effect (effect (gain :credit (* 2 (get card :advance-counter 0))) (trash card))}]}})
