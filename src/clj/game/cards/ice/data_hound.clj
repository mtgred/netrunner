(in-ns 'game.cards.ice)

(def card-definition-data-hound
  {"Data Hound"
   (letfn [(dh-trash [cards]
             {:prompt "Choose a card to trash"
              :choices cards
              :async true
              :msg (msg "trash " (:title target))
              :effect (req (do (trash state side target {:unpreventable true})
                               (continue-ability
                                 state side
                                 (reorder-choice
                                   :runner :runner (remove-once #(= % target) cards)
                                   '() (count (remove-once #(= % target) cards))
                                   (remove-once #(= % target) cards))
                                 card nil)))})]
     {:subroutines [(trace-ability 2 {:async true
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
                                                           (effect-completed state side eid)))))})]})})
