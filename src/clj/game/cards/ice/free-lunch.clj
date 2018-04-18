(in-ns 'game.core)

(def card-definitions-ice-free-lunch
  {"Free Lunch"
   {:abilities [(power-counter-ability {:label "Runner loses 1 [Credits]"
                                        :msg "make the Runner lose 1 [Credits]"
                                        :effect (effect (lose :runner :credit 1))})]
    :subroutines [add-power-counter]}})
