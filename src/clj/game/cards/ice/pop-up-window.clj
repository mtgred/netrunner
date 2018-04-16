(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-pop-up-window
  {"Pop-up Window"
   {:implementation "Encounter effect is manual. Runner choice is not implemented"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]
    :runner-abilities [(runner-break [:credit 1] 1)]}})
