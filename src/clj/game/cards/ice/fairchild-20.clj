(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-fairchild-20
  {"Fairchild 2.0"
   {:subroutines [{:label "Force the Runner to pay 2 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 2 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 2 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 2 [Credits]")
                                  (do (pay state side card :credit 2)
                                      (system-msg state side "pays 2 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 2] 2)]}})
