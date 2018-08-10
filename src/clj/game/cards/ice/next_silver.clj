(in-ns 'game.cards.ice)

(def card-definition-next-silver
  {"NEXT Silver"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain "
                           (count (filter #(and (is-type? % "ICE")
                                                (has-subtype? % "NEXT"))
                                          (all-active-installed state :corp)))
                           " subroutines")}]
    :subroutines [end-the-run]}})
