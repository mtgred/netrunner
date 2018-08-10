(in-ns 'game.cards.ice)

(def card-definition-little-engine
  {"Little Engine"
   {:subroutines [end-the-run
                  {:msg "make the Runner gain 5 [Credits]"
                   :effect (effect (gain-credits :runner 5))}]}})
