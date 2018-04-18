(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-next-opal
  {"NEXT Opal"
   {:subroutines [{:label "Install a card from HQ, paying all costs"
                   :prompt "Choose a card in HQ to install"
                   :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (in-hand? %)
                                        (= (:side %) "Corp"))}
                   :effect (effect (corp-install target nil))
                   :msg (msg (corp-install-msg target))}]}})
