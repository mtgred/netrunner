(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-nerine-20
  {"Nerine 2.0"
   {:subroutines [{:label "Do 1 brain damage and Corp may draw 1 card"
                   :delayed-completion true
                   :msg "do 1 brain damage"
                   :effect (req (when-completed (damage state :runner :brain 1 {:card card})
                                                (resolve-ability state side
                                                  {:optional
                                                   {:prompt "Draw 1 card?"
                                                    :yes-ability {:msg "draw 1 card"
                                                                  :effect (effect (draw))}
                                                    :no-ability {:effect (req (effect-completed state side eid))}}}
                                                 card nil)))}]
    :runner-abilities [(runner-break [:click 2] 2)]}})