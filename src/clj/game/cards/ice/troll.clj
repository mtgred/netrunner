(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-troll
  {"Troll"
   {:implementation "Encounter effect is manual"
    :abilities [(trace-ability 2 {:label "Force the Runner to lose [Click] or end the run"
                                  :msg "force the Runner to lose [Click] or end the run"
                                  :player :runner
                                  :prompt "Choose one"
                                  :choices ["Lose [Click]" "End the run"]
                                  :effect (req (if-not (and (= target "Lose [Click]")
                                                            (can-pay? state :runner nil [:click 1]))
                                                 (do (end-run state side)
                                                     (system-msg state side "ends the run"))
                                                 (do (lose state side :click 1)
                                                     (system-msg state side "loses [Click]"))))})]}})