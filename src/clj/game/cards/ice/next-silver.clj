(in-ns 'game.core)

(def card-definitions-ice-next-silver
  {"NEXT Silver"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(and (is-type? % "ICE")
                                                        (has-subtype? % "NEXT"))
                                                  (all-active-installed state :corp))) " subroutines")}]
    :subroutines [end-the-run]}})
