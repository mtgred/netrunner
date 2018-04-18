(in-ns 'game.core)

(def card-definitions-events-making-an-entrance
  {"Making an Entrance"
   (letfn [(entrance-trash [cards]
             {:prompt "Choose a card to trash"
              :choices (cons "None" cards)
              :delayed-completion true
              :msg (req (when (not= target "None") (str "trash " (:title target))))
              :effect (req (if (= target "None")
                             (if (not-empty cards)
                               (continue-ability state side (reorder-choice :runner :corp cards '()
                                                                            (count cards) cards) card nil)
                               (do (clear-wait-prompt state :corp)
                                   (effect-completed state side eid card)))
                             (do (trash state side target {:unpreventable true})
                                 (continue-ability state side (entrance-trash (remove-once #(= % target) cards))
                                                   card nil))))})]
     {:msg "look at and trash or rearrange the top 6 cards of their Stack"
      :delayed-completion true
      :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                   (let [from (take 6 (:deck runner))]
                     (continue-ability state side (entrance-trash from) card nil)))})})
