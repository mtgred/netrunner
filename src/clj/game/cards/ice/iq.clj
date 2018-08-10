(in-ns 'game.cards.ice)

(def card-definition-iq
  {"IQ"
   {:effect (req (add-watch state (keyword (str "iq" (:cid card)))
                            (fn [k ref old new]
                              (let [handsize (count (get-in new [:corp :hand]))]
                                (when (not= (count (get-in old [:corp :hand])) handsize)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus handsize))
                                  (update-ice-strength ref side (get-card ref card)))))))
    :subroutines [end-the-run]
    :strength-bonus (req (count (:hand corp)))
    :rez-cost-bonus (req (count (:hand corp)))
    :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))}})
