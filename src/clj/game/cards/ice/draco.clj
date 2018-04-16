(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-draco
  {"Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-ice-strength card))
    :strength-bonus (req (get-in card [:counter :power] 0))
    :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                    :msg "give the Runner 1 tag and end the run"
                                    :delayed-completion true
                                    :effect (effect (tag-runner :runner eid 1)
                                                    (end-run))})]}})
