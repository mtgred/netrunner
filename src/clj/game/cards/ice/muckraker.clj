(in-ns 'game.core)

(def card-definitions-ice-muckraker
  {"Muckraker"
   {:effect take-bad-pub
    :subroutines [(tag-trace 1)
                  (tag-trace 2)
                  (tag-trace 3)
                  end-the-run-if-tagged]}})
