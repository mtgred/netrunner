(in-ns 'game.core)

(def card-definitions-hardware-brain-chip
  {"Brain Chip"
   (let [runner-points (fn [s] (max (get-in s [:runner :agenda-point] 0) 0))]
     {:effect (req (gain state :runner
                         :memory (runner-points @state)
                         :hand-size-modification (runner-points @state))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                          (fn [k ref old new]
                            (let [bonus (- (runner-points new) (runner-points old))]
                              (when (not= 0 bonus)
                               (gain state :runner
                                     :memory bonus
                                     :hand-size-modification bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :runner
                             :memory (runner-points @state)
                             :hand-size-modification (runner-points @state)))})})
