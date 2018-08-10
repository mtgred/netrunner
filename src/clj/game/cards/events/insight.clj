(in-ns 'game.cards.events)

(def card-definition-insight
  {"Insight"
   {:async true
    :effect (req
             (let [from (take 4 (:deck corp))]
               (when (pos? (count from))
                 (do (show-wait-prompt state :runner
                                       (str "Corp to rearrange the top " (count from) " cards of R&D"))
                     (wait-for
                      (resolve-ability state :corp (reorder-choice :corp from) card targets)
                      (do (clear-wait-prompt state :runner)
                          (system-msg state :runner (str " reveals (top:) " ; here (get-in @state ...) is needed to refresh ref. to deck after reordering
                                                         (join ", " (map :title (take 4 (get-in @state [:corp :deck])))) " from the top of R&D"))
                          (effect-completed state side eid)))))))}})
