(in-ns 'game.cards.events)

(def card-definition-data-breach
  {"Data Breach"
   {:req (req rd-runnable)
    :async true
    :effect (req (let [db-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] db-eid)
                                    (assoc card :zone '(:discard)))
                   (wait-for (game.core/run state side db-eid :rd nil card)
                             (let [card (get-card state (assoc card :zone '(:discard)))]
                               (unregister-events state side card)
                               (when (:run-again card)
                                 (game.core/run state side db-eid :rd nil card))
                               (update! state side (dissoc card :run-again))))))
    :events {:successful-run-ends
             {:optional {:req (req (= [:rd] (:server target)))
                         :prompt "Make another run on R&D?"
                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                       (update! (assoc card :run-again true)))}}}}}})
