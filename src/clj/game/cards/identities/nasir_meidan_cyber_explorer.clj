(in-ns 'game.cards.identities)

(def card-definition-nasir-meidan-cyber-explorer
  {"Nasir Meidan: Cyber Explorer"
   {:events {:rez {:req (req (and (:run @state)
                                  ;; check that the rezzed item is the encountered ice
                                  (= (:cid target)
                                     (:cid (get-card state current-ice)))))
                   :effect (req (toast state :runner "Click Nasir Meidan: Cyber Explorer to lose all credits and gain credits equal to the rez cost of the newly rezzed ice." "info"))}}
    :abilities [{:req (req (and (:run @state)
                                (:rezzed (get-card state current-ice))))
                 :effect (req (let [current-ice (get-card state current-ice)]
                                (trigger-event state side :pre-rez-cost current-ice)
                                (let [cost (rez-cost state side current-ice)]
                                  (lose-credits state side (:credit runner))
                                  (gain-credits state side cost)
                                  (system-msg state side (str "loses all credits and gains " cost
                                                              " [Credits] from the rez of " (:title current-ice)))
                                  (swap! state update-in [:bonus] dissoc :cost))))}]}})
