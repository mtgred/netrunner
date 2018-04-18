(in-ns 'game.core)

(def card-definitions-events-system-seizure
  {"System Seizure"
   (let [ss {:effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}]
     {:effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
      :events {:pump-breaker {:silent (req true)
                              :req (req (not (get-in @state [:per-turn (:cid card)])))
                              :effect (effect (update! (update-in (second targets) [:pump :all-run] (fnil #(+ % (first targets)) 0)))
                                              (update-breaker-strength (second targets)))}
               :pass-ice ss :run-ends ss}
      :move-zone (req (when (= [:discard] (:zone card))
                        (unregister-events state side card)))})})
