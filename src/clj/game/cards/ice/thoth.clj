(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-thoth
  {"Thoth"
   {:implementation "Encounter effect is manual"
    :runner-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Thoth")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Runner tag"
                                    :delayed-completion true
                                    :msg (msg "do " (:tag runner) " net damage")
                                    :effect (effect (damage eid :net (:tag runner) {:card card}))})
                  (trace-ability 4 {:label "Runner loses 1 [Credits] for each tag"
                                    :delayed-completion true
                                    :msg (msg "force the Runner to lose " (:tag runner) " [Credits]")
                                    :effect (effect (lose :runner :credit (:tag runner)))})]}})
