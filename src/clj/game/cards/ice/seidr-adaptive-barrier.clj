(in-ns 'game.core)

(def card-definitions-ice-seidr-adaptive-barrier
  {"Seidr Adaptive Barrier"
   {:effect (req (let [srv (second (:zone card))]
                   (add-watch state (keyword (str "sab" (:cid card)))
                              (fn [k ref old new]
                                (let [ices (count (get-in new [:corp :servers srv :ices]))]
                                  (when (not= (count (get-in old [:corp :servers srv :ices])) ices)
                                    (update! ref side (assoc (get-card ref card) :strength-bonus ices))
                                    (update-ice-strength ref side (get-card ref card))))))))
    :strength-bonus (req (count (:ices (card->server state card))))
    :leave-play (req (remove-watch state (keyword (str "sab" (:cid card)))))
    :subroutines [end-the-run]}})
