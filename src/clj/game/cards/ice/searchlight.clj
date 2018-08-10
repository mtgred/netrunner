(in-ns 'game.cards.ice)

(def card-definition-searchlight
  {"Searchlight"
   {:advanceable :always
    :subroutines [(tag-trace advance-counters)]}})
