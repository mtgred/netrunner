(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-fairchild-10
  {"Fairchild 1.0"
   {:subroutines [{:label "Force the Runner to pay 1 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 1 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 1 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 1 [Credits]")
                                  (do (pay state side card :credit 1)
                                      (system-msg state side "pays 1 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}]
    :runner-abilities [(runner-break [:click 1] 1)]}})