(in-ns 'game.core)

(def card-definitions-ice-shadow
  {"Shadow"
   {:advanceable :always
    :subroutines [(gain-credits 2)
                  (tag-trace 3)]
    :strength-bonus advance-counters}})
