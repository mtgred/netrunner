(in-ns 'game.core)

(declare run-event)

(def card-events-trade-in
  {"Trade-In"
   {:additional-cost [:hardware 1]
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:runner-trash {:effect (effect (gain :credit (quot (:cost target) 2))
                                            (system-msg (str "trashes " (:title target) " and gains " (quot (:cost target) 2) " [Credits]"))
                                            (continue-ability {:prompt "Choose a Hardware to add to your Grip from your Stack"
                                                               :choices (req (filter #(is-type? % "Hardware")
                                                                                     (:deck runner)))
                                                               :msg (msg "add " (:title target) " to their Grip")
                                                               :effect (effect (trigger-event :searched-stack nil)
                                                                               (shuffle! :deck)
                                                                               (move target :hand)
                                                                               (unregister-events card))} card nil))}}}})
