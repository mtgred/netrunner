(in-ns 'game.core)

(declare can-host?)

(def card-programs-deep-thought
  {"Deep Thought"
   {:events {:successful-run {:silent (req true)
                              :effect (effect (add-counter card :virus 1))
                              :req (req (= target :rd))}
             :runner-turn-begins
                             {:req (req (>= (get-virus-counters state side card) 3)) :msg "look at the top card of R&D"
                              :effect (effect (prompt! card (str "The top card of R&D is "
                                                                 (:title (first (:deck corp)))) ["OK"] {}))}}}})
