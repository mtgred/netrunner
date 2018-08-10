(in-ns 'game.cards.ice)

(def card-definition-oduduwa
  {"Oduduwa"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement counter on Oduduwa"
                 :msg (msg "place 1 advancement counter on Oduduwa")
                 :effect (req (add-prop state side card :advance-counter 1 {:placed true}))}
                {:label "Place X advancement token on another piece of ice"
                 :msg (msg "place " (get-counters card :advancement) " advancement token on " (card-str state target))
                 :choices {:req ice?
                           :not-self true}
                 :effect (req (add-prop state side target :advance-counter (get-counters card :advancement) {:placed true}))}]
    :subroutines [end-the-run]}})
