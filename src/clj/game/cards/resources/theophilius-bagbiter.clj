(in-ns 'game.core)

(def card-definitions-resources-theophilius-bagbiter
  {"Theophilius Bagbiter"
   {:effect (req (lose state :runner :credit :all :run-credit :all)
                 (add-watch state :theophilius-bagbiter
                            (fn [k ref old new]
                              (let [credit (get-in new [:runner :credit])]
                                (when (not= (get-in old [:runner :credit]) credit)
                                  (swap! ref assoc-in [:runner :hand-size-base] credit))))))
    :leave-play (req (remove-watch state :theophilius-bagbiter)
                     (swap! state assoc-in [:runner :hand-size-base] 5))}})
