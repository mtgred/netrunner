(in-ns 'game.cards.ice)

(def card-definition-woodcutter
  {"Woodcutter"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [(do-net-damage 1)]}})
