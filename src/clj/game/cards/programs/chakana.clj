(in-ns 'game.cards.programs)

(def card-definition-chakana
  {"Chakana"
   {:leave-play (effect (update-all-advancement-costs))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :virus 1))}
             :pre-advancement-cost {:req (req (>= (get-virus-counters state side card) 3))
                                    :effect (effect (advancement-cost-bonus 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (= (:cid target) (:cid card))))
              :effect (effect (update-all-advancement-costs))}
             :purge {:effect (effect (update-all-advancement-costs))}}}})
