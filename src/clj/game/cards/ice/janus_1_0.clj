(in-ns 'game.cards.ice)

(def card-definition-janus-1-0
  {"Janus 1.0"
   {:subroutines [(do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 1] 1)]}})
