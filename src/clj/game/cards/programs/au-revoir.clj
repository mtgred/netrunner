(in-ns 'game.core)

(def card-definitions-programs-au-revoir
  {"Au Revoir"
   {:events {:jack-out {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}}}})
