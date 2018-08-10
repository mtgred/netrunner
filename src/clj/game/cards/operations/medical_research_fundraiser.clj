(in-ns 'game.cards.operations)

(def card-definition-medical-research-fundraiser
  {"Medical Research Fundraiser"
   {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
    :effect (effect (gain-credits 8) (gain-credits :runner 3))}})
