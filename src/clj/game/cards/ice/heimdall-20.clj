(in-ns 'game.core)

(def card-definitions-ice-heimdall-20
  {"Heimdall 2.0"
   {:subroutines [(do-brain-damage 1)
                  {:msg "do 1 brain damage and end the run" :effect (effect (damage eid :brain 1 {:card card}) (end-run))}
                  end-the-run]
    :runner-abilities [(runner-break [:click 2] 2)]}})
