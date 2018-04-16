(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-gyri-labyrinth
  {"Gyri Labyrinth"
   {:implementation "Hand size is not restored if trashed or derezzed after firing"
    :subroutines [{:req (req (:run @state))
                   :label "Reduce Runner's maximum hand size by 2 until start of next Corp turn"
                   :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                   :effect (effect (lose :runner :hand-size-modification 2)
                                   (register-events {:corp-turn-begins
                                                     {:msg "increase the Runner's maximum hand size by 2"
                                                      :effect (effect (gain :runner :hand-size-modification 2)
                                                                      (unregister-events card))}} card))}]
    :events {:corp-turn-begins nil}}})