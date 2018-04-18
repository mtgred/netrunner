(in-ns 'game.core)

(def card-definitions-ice-zed-20
  {"Zed 2.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [trash-hardware
                  (do-brain-damage 2)]
    :runner-abilities [(runner-break [:click 2] 2)]}})
