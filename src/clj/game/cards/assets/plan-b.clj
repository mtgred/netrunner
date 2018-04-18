(in-ns 'game.core)

(def card-definitions-assets-plan-b
  {"Plan B"
   (advance-ambush
    0
    {:req (req (pos? (:advance-counter (get-card state card) 0)))
     :effect
     (req (show-wait-prompt state :runner "Corp to select an agenda to score with Plan B")
          (doseq [ag (filter #(is-type? % "Agenda") (get-in @state [:corp :hand]))]
            (update-advancement-cost state side ag))
          (resolve-ability state side
            {:prompt "Select an Agenda in HQ to score"
             :choices {:req #(and (is-type? % "Agenda")
                                  (<= (:current-cost %) (:advance-counter (get-card state card) 0))
                                  (in-hand? %))}
             :msg (msg "score " (:title target))
             :effect (effect (score (assoc target :advance-counter
                                           (:current-cost target)))
                             (clear-wait-prompt :runner))}
           card nil))}
    "Score an Agenda from HQ?")})
