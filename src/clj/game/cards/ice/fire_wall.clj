(in-ns 'game.cards.ice)

(def card-definition-fire-wall
  {"Fire Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
