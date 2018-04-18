(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-next-silver
  {"NEXT Silver"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(and (is-type? % "ICE")
                                                        (has-subtype? % "NEXT"))
                                                  (all-active-installed state :corp))) " subroutines")}]
    :subroutines [end-the-run]}})
