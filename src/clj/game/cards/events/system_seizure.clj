(in-ns 'game.cards.events)

(def card-definition-system-seizure
  {"System Seizure"
  {:effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
   :events {:pump-breaker {:silent (req true)
                           :req (req (or (and (has-flag? state side :current-run :system-seizure)
                                              (run-flag? state side (second targets) :system-seizure))
                                         (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (req (update! state side (update-in (second targets) [:pump :all-run] (fnil #(+ % (first targets)) 0)))
                                        (register-run-flag! state side card :system-seizure (fn [_ _ c] (= (:cid c) (:cid (second targets)))))
                                        (update-breaker-strength state side (second targets))
                                        (swap! state assoc-in [:per-turn (:cid card)] targets))}}
   :move-zone (req (when (= [:discard] (:zone card))
                     (unregister-events state side card)))}})
