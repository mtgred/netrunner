(in-ns 'game.core)

(def card-operations-medical-research-fundraiser
  {"Medical Research Fundraiser"
   {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
    :effect (effect (gain :credit 8) (gain :runner :credit 3))}})
