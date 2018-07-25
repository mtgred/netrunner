(in-ns 'game.cards.ice)

(def card-definition-shadow
  {"Shadow"
   {:advanceable :always
    :subroutines [(gain-credits-sub 2)
                  (tag-trace 3)]
    :strength-bonus advance-counters}})
