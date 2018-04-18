(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-universal-connectivity-fee
  {"Universal Connectivity Fee"
   {:subroutines [{:label "Force the Runner to lose credits"
                   :msg (msg "force the Runner to lose " (if tagged "all credits" "1 [Credits]"))
                   :effect (req (if tagged
                                  (do (lose state :runner :credit :all :run-credit :all)
                                      (when current-ice
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (trash state side card))
                                  (lose state :runner :credit 1)))}]}})
