(in-ns 'game.core)

(def card-definitions-ice-ichi-10
  {"Ichi 1.0"
   {:subroutines [trash-program
                  (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :delayed-completion true
                                    :effect (req (when-completed (damage state :runner :brain 1 {:card card})
                                                                 (tag-runner state :runner eid 1)))})]
    :runner-abilities [(runner-break [:click 1] 1)]}})
