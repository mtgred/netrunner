(in-ns 'game.cards.ice)

(def card-definition-zed-1-0
  {"Zed 1.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [(do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 1] 1)]}})
