(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-neutralize-all-threats
  {"Neutralize All Threats"
   {:in-play [:hq-access 1]
    :events {:pre-access {:req (req (and (= target :archives)
                                         (seq (filter #(:trash %) (:discard corp)))))
                          :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}
             :access {:effect (req (swap! state assoc-in [:runner :register :force-trash] false))}
             :pre-trash {:req (req (let [cards (map first (turn-events state side :pre-trash))]
                                     (and (empty? (filter #(:trash %) cards))
                                          (number? (:trash target)))))
                         :once :per-turn
                         :effect (req (swap! state assoc-in [:runner :register :force-trash] true))}}}})
