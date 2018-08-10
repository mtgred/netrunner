(in-ns 'game.cards.ice)

(def card-definition-hadrian-s-wall
  {"Hadrian's Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
