(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-chimera
  {"Chimera"
   (let [turn-end-ability {:effect (effect (derez :corp card)
                                           (update! (assoc (get-card state card) :subtype "Mythic")))}]
     {:prompt "Choose one subtype"
      :choices ["Barrier" "Code Gate" "Sentry"]
      :msg (msg "make it gain " target " until the end of the turn")
      :effect (effect (update! (assoc card
                                 :subtype-target target
                                 :subtype (combine-subtypes true (:subtype card) target)))
                      (update-ice-strength card))
      :events {:runner-turn-ends turn-end-ability
               :corp-turn-ends turn-end-ability}
      :subroutines [end-the-run]})})
