(in-ns 'game.cards.ice)

(def card-definition-masvingo
  {"Masvingo"
   {:implementation "Number of subs is manual"
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :effect (effect (add-prop card :advance-counter 1))
    :subroutines [end-the-run]}})
