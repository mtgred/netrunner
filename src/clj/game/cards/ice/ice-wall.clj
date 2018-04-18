(in-ns 'game.core)

(def card-definitions-ice-ice-wall
  {"Ice Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
