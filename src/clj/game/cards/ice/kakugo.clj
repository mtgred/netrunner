(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-kakugo
  {"Kakugo"
   {:events {:pass-ice {:delayed-completion true
                        :req (req (= target card))
                        :msg "do 1 net damage"
                        :effect (effect (damage eid :net 1 {:card card}))}}
    :subroutines [end-the-run]}})
