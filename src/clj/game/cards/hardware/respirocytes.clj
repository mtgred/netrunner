(in-ns 'game.core)

(def card-hardware-respirocytes
  {"Respirocytes"
   (let [ability {:once :per-turn
                  :msg "draw 1 card and add a power counter to itself"
                  :effect (req (draw state :runner)
                               (add-counter state side (get-card state card) :power 1)
                               (when (= (get-in (get-card state card) [:counter :power]) 3)
                                 (system-msg state :runner "trashes Respirocytes as it reached 3 power counters")
                                 (trash state side card {:unpreventable true})))}]
   {:effect (req (let [watch-id (keyword "respirocytes" (str (:cid card)))]
                   (update! state side (assoc card :respirocytes-watch-id watch-id))
                   (add-watch state watch-id
                            (fn [k ref old new]
                              (when (and (seq (get-in old [:runner :hand]))
                                         (empty? (get-in new [:runner :hand])))
                                (resolve-ability ref side ability card nil)))))
                 (damage state side eid :meat 1 {:unboostable true :card card}))
    :msg "suffer 1 meat damage"
    :trash-effect {:effect (req (remove-watch state (:respirocytes-watch-id card)))}
    :leave-play (req (remove-watch state (:respirocytes-watch-id card)))
    :events {:runner-turn-begins {:req (req (empty? (get-in @state [:runner :hand])))
                                  :effect (effect (resolve-ability ability card nil))}
             :corp-turn-begins {:req (req (empty? (get-in @state [:runner :hand])))
                                :effect (effect (resolve-ability ability card nil))}}})})