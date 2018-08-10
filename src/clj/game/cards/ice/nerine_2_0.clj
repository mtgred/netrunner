(in-ns 'game.cards.ice)

(def card-definition-nerine-2-0
  {"Nerine 2.0"
   {:subroutines [{:label "Do 1 brain damage and Corp may draw 1 card"
                   :async true
                   :msg "do 1 brain damage"
                   :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                          (resolve-ability
                                            state side
                                            {:optional
                                             {:prompt "Draw 1 card?"
                                              :yes-ability {:async true
                                                            :msg "draw 1 card"
                                                            :effect (effect (draw eid 1 nil))}}}
                                            card nil)))}]
    :runner-abilities [(runner-break [:click 2] 2)]}})
