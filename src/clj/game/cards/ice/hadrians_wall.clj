(in-ns 'game.cards.ice)

(def card-definition-hadrians-wall
  {"Hadrian's Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
