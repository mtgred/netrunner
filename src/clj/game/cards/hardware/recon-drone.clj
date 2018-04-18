(in-ns 'game.core)

(def card-definitions-hardware-recon-drone
  {"Recon Drone"
   ; eventmap uses reverse so we get the most recent event of each kind into map
   (let [eventmap (fn [s] (into {} (reverse (get s :turn-events))))]
     {:abilities [{:req (req (and (true? (:access @state)) (= (:cid (second (:pre-damage (eventmap @state))))
                                                              (:cid (first (:post-access-card (eventmap @state)))))))
                :effect (effect (resolve-ability
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min (last (:pre-damage (eventmap @state)))
                                                               (:credit runner)))}
                                   :msg (msg "prevent " target " damage")
                                   :effect (effect (damage-prevent (first (:pre-damage (eventmap @state))) target)
                                                   (lose :credit target)
                                                   (trash card {:cause :ability-cost}))} card nil))}]
     :events    {:pre-access {:effect (req (doseq [dtype [:net :brain :meat]] (swap! state update-in [:prevent :damage dtype] #(conj % card))))}
                 :run-ends   {:effect (req (doseq [dtype [:net :brain :meat]] (swap! state update-in [:prevent :damage dtype] #(drop 1 %))))}}})})
