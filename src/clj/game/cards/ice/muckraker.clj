(in-ns 'game.cards.ice)

(def card-definition-muckraker
  {"Muckraker"
   {:effect take-bad-pub
    :subroutines [(tag-trace 1)
                  (tag-trace 2)
                  (tag-trace 3)
                  end-the-run-if-tagged]}})
