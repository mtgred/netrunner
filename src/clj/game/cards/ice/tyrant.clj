(in-ns 'game.cards.ice)

(def card-definition-tyrant
  {"Tyrant"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [end-the-run]}})
