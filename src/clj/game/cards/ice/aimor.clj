(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-aimor
  {"Aimor"
   {:subroutines [{:label "Trash the top 3 cards of the Stack. Trash Aimor."
                   :effect (req (when (not-empty (:deck runner))
                                  (system-msg state :corp
                                              (str "uses Aimor to trash "
                                                   (join ", " (map :title (take 3 (:deck runner))))
                                                   " from the Runner's Stack"))
                                  (mill state :corp :runner 3))
                                (when current-ice
                                  (no-action state :corp nil)
                                  (continue state :runner nil))
                                (trash state side card)
                                (system-msg state side (str "trashes Aimor")))}]}})
