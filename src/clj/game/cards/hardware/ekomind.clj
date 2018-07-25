(in-ns 'game.cards.hardware)

(def card-definition-ekomind
  {"Ekomind"
   (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
     {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                   (add-watch state :ekomind (fn [k ref old new]
                                               (let [hand-size (count (get-in new [:runner :hand]))]
                                                 (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                   (update-base-mu ref hand-size))))))
      :leave-play (req (remove-watch state :ekomind))})})
