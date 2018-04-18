(in-ns 'game.core)

(def card-definitions-ice-matrix-analyzer
  {"Matrix Analyzer"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req can-be-advanced?}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}]
    :subroutines [(tag-trace 2)]}})
