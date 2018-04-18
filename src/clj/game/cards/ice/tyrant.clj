(in-ns 'game.core)

(def card-definitions-ice-tyrant
  {"Tyrant"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [end-the-run]}})
