(in-ns 'game.cards.assets)

(def card-definition-plan-b
  {"Plan B"
   (advance-ambush
    0
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :effect (req (show-wait-prompt state :runner "Corp to select an agenda to score with Plan B")
                  (doseq [ag (filter #(is-type? % "Agenda") (:hand corp))]
                    (update-advancement-cost state side ag))
                  (resolve-ability
                    state side
                    {:prompt "Select an Agenda in HQ to score"
                     :choices {:req #(and (is-type? % "Agenda")
                                          (<= (:current-cost %) (get-counters (get-card state card) :advancement))
                                          (in-hand? %))}
                     :msg (msg "score " (:title target))
                     :effect (effect (score (assoc target :advance-counter
                                                   (:current-cost target)))
                                     (clear-wait-prompt :runner))}
                    card nil))}
    "Score an Agenda from HQ?")})
