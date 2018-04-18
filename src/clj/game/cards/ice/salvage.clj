(in-ns 'game.core)

(def card-definitions-ice-salvage
  {"Salvage"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [(tag-trace 2)]}})
