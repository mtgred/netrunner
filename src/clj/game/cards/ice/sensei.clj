(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-sensei
  {"Sensei"
   {:subroutines [{:label "Give each other ICE encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give each other ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}})
