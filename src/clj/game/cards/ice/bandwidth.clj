(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-bandwidth
  {"Bandwidth"
   {:subroutines [{:msg "give the Runner 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-runner :runner eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose :runner :tag 1))
                                                       :msg "make the Runner lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}})
