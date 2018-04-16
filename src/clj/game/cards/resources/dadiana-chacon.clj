(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-dadiana-chacon
  {"Dadiana Chacon"
   (let [trashme {:effect (effect (system-msg "trashes Dadiana Chacon and suffers 3 meat damage")
                                  (register-events {:play {:req (req (= "Runner" (:side target)))
                                                           :effect (effect (unregister-events card)
                                                                           (damage eid :meat 3 {:unboostable true :card card})
                                                                           (trash card {:cause :ability-cost}))}} card))}
         ability {:once :per-turn
                  :msg "gain 1 [Credits]"
                  :req (req (< (get-in @state [:runner :credit]) 6))
                  :effect (req (gain state :runner :credit 1))}]
     {:effect (req (if (zero? (get-in @state [:runner :credit]))
                     (resolve-ability state side trashme card nil)
                     (add-watch state :dadiana
                                (fn [k ref old new]
                                  (when (and (not (zero? (get-in old [:runner :credit])))
                                             (zero? (get-in new [:runner :credit])))
                                    (resolve-ability ref side trashme card nil))))))
      :leave-play (req (remove-watch state :dadiana))
      :flags {:drip-economy true}
      :events {:play nil
               :runner-turn-begins ability}})})