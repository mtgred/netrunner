(in-ns 'game.core)

(def card-hardware-ekomind
  {"Ekomind"
   {:effect (req (swap! state assoc-in [:runner :memory] (count (get-in @state [:runner :hand])))
                 (add-watch state :ekomind (fn [k ref old new]
                                             (let [hand-size (count (get-in new [:runner :hand]))]
                                               (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                 (swap! ref assoc-in [:runner :memory] hand-size))))))
    :leave-play (req (remove-watch state :ekomind))}})
