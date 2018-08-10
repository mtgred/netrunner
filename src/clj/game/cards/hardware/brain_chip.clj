(in-ns 'game.cards.hardware)

(def card-definition-brain-chip
  {"Brain Chip"
   (let [runner-points (fn [s] (max (get-in s [:runner :agenda-point] 0) 0))]
     {:effect (req (gain state :runner
                         :memory (runner-points @state)
                         :hand-size (runner-points @state))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                              (fn [k ref old new]
                                (let [bonus (- (runner-points new) (runner-points old))]
                                  (when-not (zero? bonus)
                                    (gain state :runner
                                          :memory bonus
                                          :hand-size bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :runner
                             :memory (runner-points @state)
                             :hand-size (runner-points @state)))})})
