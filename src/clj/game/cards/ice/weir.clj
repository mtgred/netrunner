(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-weir
  {"Weir"
   {:subroutines [{:label "force the Runner to lose 1 [Click], if able"
                   :msg "force the Runner to lose 1 [Click]"
                   :effect runner-loses-click}
                  {:label "Runner trashes 1 card from their Grip"
                   :req (req (pos? (count (:hand runner))))
                   :prompt "Choose a card to trash from your Grip"
                   :player :runner
                   :choices (req (:hand runner))
                   :not-distinct true
                   :effect (effect (trash :runner target)
                                   (system-msg :runner (str "trashes " (:title target) " from their Grip")))}]}})
