(in-ns 'game.core)

(declare run-event)

(def card-events-showing-off
  {"Showing Off"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :delayed-completion true
                        :effect (req (when-completed (resolve-ability state side
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