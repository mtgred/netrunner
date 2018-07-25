(in-ns 'game.cards.resources)

(def card-definition-psych-mike
  {"Psych Mike"
   {:events {:successful-run-ends
             {:req (req (and (= [:rd] (:server target))
                             (first-event? state side :successful-run-ends)))
              :effect (effect (gain-credits :runner (total-cards-accessed target :deck)))}}}})
