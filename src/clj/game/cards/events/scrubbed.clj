(in-ns 'game.core)

(declare run-event)

(def card-events-scrubbed
  {"Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-ice {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target))
                                                  (update-ice-strength current-ice))}
                  :pre-ice-strength {:req (req (= (:cid target) (get-in card [:scrubbed-target :cid])))
                                     :effect (effect (ice-strength-bonus -2 target))}
                  :run-ends sc})}})
