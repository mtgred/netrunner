(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-next-bronze
  {"NEXT Bronze"
   {:subroutines [end-the-run]
    :strength-bonus (req (next-ice-count corp))
    :events (let [nb {:req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "NEXT")))
                      :effect (effect (update-ice-strength card))}]
              {:rez nb :derez nb :trash nb :card-moved nb})}})
