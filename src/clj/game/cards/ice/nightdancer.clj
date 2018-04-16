(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-nightdancer
  {"Nightdancer"
   {:subroutines [{:label "The Runner loses [Click], if able. You have an additional [Click] to spend during your next turn."
                   :msg "force the runner to lose a [Click], if able. Corp gains an additional [Click] to spend during their next turn"
                   :effect (req
                             (lose state :runner :click 1)
                             (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]}})