(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-sandman
  {"Sandman"
   {:subroutines [{:label "Add an installed Runner card to the grip"
                   :req (req (not-empty (all-installed state :runner)))
                   :effect (effect (show-wait-prompt :runner "Corp to select Sandman target")
                                   (resolve-ability {:choices {:req #(and (installed? %)
                                                                           (= (:side %) "Runner"))}
                                                      :msg (msg "to add " (:title target) " to the grip")
                                                      :effect (effect (clear-wait-prompt :runner)
                                                                      (move :runner target :hand true))
                                                      :cancel-effect (effect (clear-wait-prompt :runner))}
                                                     card nil))}]}})
