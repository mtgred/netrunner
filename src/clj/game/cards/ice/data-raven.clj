(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-data-raven
  {"Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag
                (power-counter-ability give-tag)]
    :runner-abilities [{:label "End the run"
                        :effect (req (end-run state :runner)
                                     (system-msg state :runner "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Raven")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}})