(in-ns 'game.core)

(def card-definitions-ice-oduduwa
  {"Oduduwa"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement counter on Oduduwa"
                 :msg (msg "place 1 advancement counter on Oduduwa")
                 :effect (req (add-prop state side card :advance-counter 1 {:placed true}))}
                {:label "Place X advancement token on another piece of ice"
                 :msg (msg "place " (:advance-counter card 0) " advancement token on " (card-str state target))
                 :choices {:req ice?
                           :not-self (req (:cid card))}
                 :effect (req (add-prop state side target :advance-counter (:advance-counter card 0) {:placed true}))}]
    :subroutines [end-the-run]}})
