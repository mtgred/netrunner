(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-woodcutter
  {"Woodcutter"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [(do-net-damage 1)]}})