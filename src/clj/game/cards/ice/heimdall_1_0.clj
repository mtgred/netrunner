(in-ns 'game.cards.ice)

(def card-definition-heimdall-1-0
  {"Heimdall 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :runner-abilities [(runner-break [:click 1] 1)]}})
