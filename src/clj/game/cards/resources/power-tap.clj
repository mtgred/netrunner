(in-ns 'game.core)

(def card-definitions-resources-power-tap
  {"Power Tap"
   {:events {:trace {:msg "gain 1 [Credits]" :effect (effect (gain :runner :credit 1))}}}})
