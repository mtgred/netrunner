(in-ns 'game.cards.programs)

(def card-definition-au-revoir
  {"Au Revoir"
   {:events {:jack-out {:effect (effect (gain-credits 1))
                        :msg "gain 1 [Credits]"}}}})
