(in-ns 'game.core)

(def card-definitions-ice-resistor
  {"Resistor"
   {:effect (req (add-watch state (keyword (str "resistor" (:cid card)))
                            (fn [k ref old new]
                              (let [tags (get-in new [:runner :tag])]
                                (when (not= (get-in old [:runner :tag]) tags)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus tags))
                                  (update-ice-strength ref side (get-card ref card)))))))
    :strength-bonus (req (:tag runner))
    :leave-play (req (remove-watch state (keyword (str "resistor" (:cid card)))))
    :subroutines [(trace-ability 4 end-the-run)]}})
