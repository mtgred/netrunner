(in-ns 'game.core)

(def card-definitions-events-rip-deal
  {"Rip Deal"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                                   {:delayed-completion true
                                    :effect (req (let [n (min (-> @state :corp :hand count) (access-count state side :hq-access))
                                                       heap (-> @state :runner :discard count (- 1))]
                                                   (move state side (find-cid (:cid card) (:discard runner)) :rfg)
                                                   (if (pos? heap)
                                                     (resolve-ability state side
                                                                      {:show-discard true
                                                                       :prompt (str "Choose " (min n heap) " card(s) to move from the Heap to your Grip")
                                                                       :delayed-completion true
                                                                       :msg (msg "take " (join ", " (map :title targets)) " from their Heap to their Grip")
                                                                       :choices {:max (min n heap)
                                                                                 :all true
                                                                                 :req #(and (= (:side %) "Runner")
                                                                                            (in-discard? %))}
                                                                       :effect (req (doseq [c targets] (move state side c :hand))
                                                                                    (do-access state side eid (:server run) {:hq-root-only true}))} card nil)
                                                     (resolve-ability state side
                                                                      {:delayed-completion true
                                                                       :msg (msg "take no cards from their Heap to their Grip")
                                                                       :effect (req (do-access state side eid (:server run) {:hq-root-only true}))} card nil))))}} card))}})
