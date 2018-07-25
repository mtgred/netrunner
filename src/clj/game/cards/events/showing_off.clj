(in-ns 'game.cards.events)

(def card-definition-showing-off
  {"Showing Off"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :async true
                        :effect (req
                                  ;; Not sure why this is done
                                  (wait-for (resolve-ability
                                              state side
                                              {:effect (effect (register-events (:events (card-def card))
                                                                                (assoc card :zone '(:discard))))}
                                              card nil)
                                            (do-access state side eid (:server run))))}} card))
    :events {:pre-access {:silent (req true)
                          :effect (req (swap! state assoc-in [:corp :deck]
                                              (rseq (into [] (get-in @state [:corp :deck])))))}
             :run-ends {:effect (req (swap! state assoc-in [:corp :deck]
                                            (rseq (into [] (get-in @state [:corp :deck]))))
                                     (unregister-events state side card))}}}})
