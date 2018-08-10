(in-ns 'game.cards.identities)

(def card-definition-cerebral-imaging-infinite-frontiers
  {"Cerebral Imaging: Infinite Frontiers"
   {:effect (req (when (> (:turn @state) 1)
                   (swap! state assoc-in [:corp :hand-size :base] (:credit corp)))
                 (add-watch state :cerebral-imaging
                            (fn [k ref old new]
                              (let [credit (get-in new [:corp :credit])]
                                (when (not= (get-in old [:corp :credit]) credit)
                                  (swap! ref assoc-in [:corp :hand-size :base] credit))))))
    :leave-play (req (remove-watch state :cerebral-imaging)
                     (swap! state assoc-in [:corp :hand-size :base] 5))}})
