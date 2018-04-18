(in-ns 'game.core)

(def card-definitions-resources-order-of-sol
  {"Order of Sol"
   {:effect (req (add-watch state :order-of-sol
                            (fn [k ref old new]
                              (when (and (not (zero? (get-in old [:runner :credit])))
                                         (zero? (get-in new [:runner :credit])))
                                (resolve-ability ref side {:msg "gain 1 [Credits]" :once :per-turn
                                                           :effect (effect (gain :credit 1))} card nil)))))
    :events {:runner-turn-begins {:req (req (= (:credit runner) 0)) :msg "gain 1 [Credits]"
                                  :effect (req (gain state :runner :credit 1)
                                               (swap! state assoc-in [:per-turn (:cid card)] true))}
             :corp-turn-begins {:req (req (= (:credit runner) 0)) :msg "gain 1 [Credits]"
                                :effect (req (gain state :runner :credit 1)
                                             (swap! state assoc-in [:per-turn (:cid card)] true))}
             :runner-install {:silent (req (pos? (:credit runner)))
                              :req (req (and (= target card) (= (:credit runner) 0))) :msg "gain 1 [Credits]"
                              :effect (req (gain state :runner :credit 1)
                                           (swap! state assoc-in [:per-turn (:cid card)] true))}}
    :leave-play (req (remove-watch state :order-of-sol))}})
