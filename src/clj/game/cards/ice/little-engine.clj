(in-ns 'game.core)

(def card-definitions-ice-little-engine
  {"Little Engine"
   {:subroutines [end-the-run
                  {:msg "make the Runner gain 5 [Credits]" :effect (effect (gain :runner :credit 5))}]}})
