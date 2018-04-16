(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-sandburg
  {"Sandburg"
   {:effect (req (add-watch state :sandburg
                            (fn [k ref old new]
                              (let [credit (get-in new [:corp :credit])]
                                (when (not= (get-in old [:corp :credit]) credit)
                                  (update-all-ice ref side)))))
                 (update-all-ice state side))
    :events {:pre-ice-strength {:req (req (and (ice? target)
                                               (>= (:credit corp) 10)))
                                :effect (effect (ice-strength-bonus (quot (:credit corp) 5) target))}}
    :leave-play (req (remove-watch state :sandburg)
                     (update-all-ice state side))}})
