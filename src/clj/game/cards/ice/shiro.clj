(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-shiro
  {"Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :delayed-completion true
                   :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck corp))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :runner)
                                        (effect-completed state side eid card)))))}
                  {:label "Force the Runner to access the top card of R&D"
                   :effect (req (doseq [c (take (get-in @state [:runner :rd-access]) (:deck corp))]
                                  (system-msg state :runner (str "accesses " (:title c)))
                                  (handle-access state side [c])))}]}})