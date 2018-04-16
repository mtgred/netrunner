(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-data-hound
  {"Data Hound"
   (letfn [(dh-trash [cards]
             {:prompt "Choose a card to trash"
              :choices cards
              :delayed-completion true
              :msg (msg "trash " (:title target))
              :effect (req (do (trash state side target {:unpreventable true})
                               (continue-ability
                                 state side
                                 (reorder-choice
                                   :runner :runner (remove-once #(= % target) cards)
                                   '() (count (remove-once #(= % target) cards))
                                   (remove-once #(= % target) cards))
                                 card nil)))})]
     {:subroutines [(trace-ability 2 {:delayed-completion true
                                      :label "Look at the top of Stack"
                                      :msg "look at top X cards of Stack"
                                      :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of the Runner's Stack")
                                                   (let [c (- target (second targets))
                                                         from (take c (:deck runner))]
                                                     (system-msg state :corp
                                                                 (str "looks at the top " c " cards of Stack"))
                                                     (if (< 1 c)
                                                       (continue-ability state side (dh-trash from) card nil)
                                                       (do (system-msg state :corp (str "trashes " (:title (first from))))
                                                           (trash state side (first from) {:unpreventable true})
                                                           (clear-wait-prompt state :runner)
                                                           (effect-completed state side eid card)))))})]})})
