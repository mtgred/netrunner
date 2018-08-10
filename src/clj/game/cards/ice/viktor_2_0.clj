(in-ns 'game.cards.ice)

(def card-definition-viktor-2-0
  {"Viktor 2.0"
   {:abilities [(power-counter-ability (do-brain-damage 1))]
    :subroutines [(trace-ability 2 add-power-counter)
                  end-the-run]
    :runner-abilities [(runner-break [:click 2] 2)]}})
