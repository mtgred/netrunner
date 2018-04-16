(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-searchlight
  {"Searchlight"
   {:advanceable :always
    ;; Could replace this with (tag-trace advance-counters).
    :subroutines [{:label "Trace X - Give the Runner 1 tag"
                   :trace {:base advance-counters
                           :delayed-completion true
                           :effect (effect (tag-runner :runner eid 1))
                           :msg "give the Runner 1 tag"}}]}})