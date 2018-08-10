(in-ns 'game.cards.ice)

(def card-definition-ice-wall
  {"Ice Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
