(in-ns 'game.core)

(def card-operations-precognition
  {"Precognition"
   {:delayed-completion true
    :msg "rearrange the top 5 cards of R&D"
    :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                 (let [from (take 5 (:deck corp))]
                   (if (pos? (count from))
                     (continue-ability state side (reorder-choice :corp :runner from '()
                                                                  (count from) from) card nil)
                     (do (clear-wait-prompt state :runner)
                         (effect-completed state side eid card)))))}})