(in-ns 'game.core)

(def card-definitions-ice-hadrians-wall
  {"Hadrians Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}})
