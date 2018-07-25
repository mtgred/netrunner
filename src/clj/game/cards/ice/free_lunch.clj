(in-ns 'game.cards.ice)

(def card-definition-free-lunch
  {"Free Lunch"
   {:abilities [(power-counter-ability {:label "Runner loses 1 [Credits]"
                                        :msg "make the Runner lose 1 [Credits]"
                                        :effect (effect (lose-credits :runner 1))})]
    :subroutines [add-power-counter]}})
