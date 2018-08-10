(in-ns 'game.cards.resources)

(def card-definition-order-of-sol
  {"Order of Sol"
   {:effect (req (add-watch state :order-of-sol
                            (fn [k ref old new]
                              (when (and (not (zero? (get-in old [:runner :credit])))
                                         (zero? (get-in new [:runner :credit])))
                                (resolve-ability ref side {:msg "gain 1 [Credits]"
                                                           :once :per-turn
                                                           :effect (effect (gain-credits 1))}
                                                 card nil)))))
    :events {:runner-turn-begins {:req (req (zero? (:credit runner)))
                                  :msg "gain 1 [Credits]"
                                  :effect (req (gain-credits state :runner 1)
                                               (swap! state assoc-in [:per-turn (:cid card)] true))}
             :corp-turn-begins {:req (req (zero? (:credit runner)))
                                :msg "gain 1 [Credits]"
                                :effect (req (gain-credits state :runner 1)
                                             (swap! state assoc-in [:per-turn (:cid card)] true))}
             :runner-install {:silent (req (pos? (:credit runner)))
                              :req (req (and (= target card)
                                             (zero? (:credit runner))))
                              :msg "gain 1 [Credits]"
                              :effect (req (gain-credits state :runner 1)
                                           (swap! state assoc-in [:per-turn (:cid card)] true))}}
    :leave-play (req (remove-watch state :order-of-sol))}})
