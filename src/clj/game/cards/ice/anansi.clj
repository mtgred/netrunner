(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-anansi
  {"Anansi"
   (let [corp-draw {:optional {:prompt "Draw 1 card?"
                               :yes-ability {:delayed-completion true
                                             :msg "draw 1 card"
                                             :effect (effect (draw eid 1 nil))}}}
         runner-draw {:player :runner
                      :optional {:prompt "Pay 2[Credits] to draw 1 card?"
                                 :no-ability {:effect (effect (system-msg :runner "does not draw 1 card"))}
                                 :yes-ability {:delayed-completion true
                                               :effect (effect
                                                         (system-msg :runner "pays 2[Credits] to draw 1 card")
                                                         (lose :credit 2)
                                                         (draw eid 1 nil))}}}]
     {:implementation "Encounter-ends effect is manually triggered."
      :subroutines [{:msg "rearrange the top 5 cards of R&D"
                     :delayed-completion true
                     :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                  (let [from (take 5 (:deck corp))]
                                       (if (pos? (count from))
                                         (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                      (count from) from)
                                                           card nil)
                                         (do (clear-wait-prompt state :runner)
                                             (effect-completed state side eid)))))}
                    {:label "Draw 1 card; allow runner to draw 1 card"
                     :delayed-completion true
                     :effect (req (when-completed (resolve-ability state side corp-draw card nil)
                                                  (continue-ability state :runner runner-draw card nil)))}
                    (do-net-damage 1)]
      :abilities [(do-net-damage 3)]})})
