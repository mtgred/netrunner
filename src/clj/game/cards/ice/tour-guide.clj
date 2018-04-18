(in-ns 'game.core)

(def card-definitions-ice-tour-guide
  {"Tour Guide"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(is-type? % "Asset")
                                                  (all-active-installed state :corp))) " subroutines")}]
    :subroutines [end-the-run]}})
