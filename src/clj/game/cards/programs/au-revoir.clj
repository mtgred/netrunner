(in-ns 'game.core)

(declare can-host?)

(def card-programs-au-revoir
  {"Au Revoir"
   {:events {:jack-out {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}}}})