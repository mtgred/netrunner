(in-ns 'game.core)

(def card-definitions-ice-zed-10
  {"Zed 1.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [(do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 1] 1)]}})
