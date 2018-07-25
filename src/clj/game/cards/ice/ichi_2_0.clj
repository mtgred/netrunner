(in-ns 'game.cards.ice)

(def card-definition-ichi-2-0
  {"Ichi 2.0"
   {:subroutines [trash-program
                  (trace-ability 3 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :async true
                                    :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                           (gain-tags state :corp eid 1)))})]
    :runner-abilities [(runner-break [:click 2] 2)]}})
