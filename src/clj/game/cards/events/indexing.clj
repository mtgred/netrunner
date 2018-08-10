(in-ns 'game.cards.events)

(def card-definition-indexing
  {"Indexing"
   {:req (req rd-runnable)
    :async true
    :effect (effect (run :rd
                         {:req (req (= target :rd))
                          :replace-access
                          {:msg "rearrange the top 5 cards of R&D"
                           :async true
                           :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of R&D")
                                        (let [from (take 5 (:deck corp))]
                                          (if (pos? (count from))
                                            (continue-ability state side (reorder-choice :corp :corp from '()
                                                                                         (count from) from) card nil)
                                            (do (clear-wait-prompt state :corp)
                                                (effect-completed state side eid)))))}}
                         card))}})
