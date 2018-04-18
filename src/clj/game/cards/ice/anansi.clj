(in-ns 'game.core)

(def card-definitions-ice-anansi
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
