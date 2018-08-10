(in-ns 'game.cards.ice)

(def card-definition-salvage
  {"Salvage"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [(tag-trace 2)]}})
