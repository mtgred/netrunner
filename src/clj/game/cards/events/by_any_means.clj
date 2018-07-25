(in-ns 'game.cards.events)

(def card-definition-by-any-means
  {"By Any Means"
   {:effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:runner-turn-ends {:effect (effect (unregister-events card))}
             :access {:req (req (not= [:discard] (:zone target)))
                      :interactive (req true)
                      :async true
                      :msg (msg "trash " (:title target) " at no cost and suffer 1 meat damage")
                      :effect (req (wait-for (trash state side (assoc target :seen true) nil)
                                             (do (swap! state assoc-in [:runner :register :trashed-card] true)
                                                 (damage state :runner eid :meat 1 {:unboostable true}))))}}}})
