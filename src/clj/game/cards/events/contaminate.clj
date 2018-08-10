(in-ns 'game.cards.events)

(def card-definition-contaminate
  {"Contaminate"
   {:effect (req (resolve-ability
                   state side
                   {:msg (msg "place 3 virus tokens on " (:title target))
                    :choices {:req #(and (installed? %)
                                         (= (:side %) "Runner")
                                         (zero? (get-virus-counters state side %)))}
                    :effect (req (add-counter state :runner target :virus 3))}
                   card nil))}})
