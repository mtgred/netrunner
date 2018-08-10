(in-ns 'game.cards.programs)

(def card-definition-ixodidae
  {"Ixodidae"
   {:events {:corp-credit-loss {:msg "gain 1 [Credits]"
                                :effect (effect (gain-credits :runner 1))}
             :purge {:effect (effect (trash card {:cause :purge}))}}}})
