(in-ns 'game.core)

(def card-definitions-ice-fire-wall
  {"Fire Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
