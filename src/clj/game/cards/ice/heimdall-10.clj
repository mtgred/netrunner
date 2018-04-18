(in-ns 'game.core)

(def card-definitions-ice-heimdall-10
  {"Heimdall 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :runner-abilities [(runner-break [:click 1] 1)]}})
