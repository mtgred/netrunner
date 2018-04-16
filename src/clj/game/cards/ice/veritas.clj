(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-veritas
  {"Veritas"
   {:subroutines [{:label "Corp gains 2 [Credits]"
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain :corp :credit 2))}
                  {:label "Runner loses 2 [Credits]"
                   :msg "force the Runner to lose 2 [Credits]"
                   :effect (effect (lose :runner :credit 2))}
                  (trace-ability 2 give-tag)]}})
