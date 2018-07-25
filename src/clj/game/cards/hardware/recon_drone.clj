(in-ns 'game.cards.hardware)

(def card-definition-recon-drone
  {"Recon Drone"
   ; eventmap uses reverse so we get the most recent event of each kind into map
   (letfn [(eventmap [s]
             (into {} (reverse (get s :turn-events))))]
     {:interactions {:prevent [{:type #{:net :brain :meat}
                                :req (req (:access @state))}]}
      :abilities [{:req (req (= (:cid (second (:pre-damage (eventmap @state))))
                                (:cid (first (:pre-access-card (eventmap @state))))))
                   :effect (effect (resolve-ability
                                     {:prompt "Choose how much damage to prevent"
                                      :priority 50
                                      :choices {:number (req (min (last (:pre-damage (eventmap @state)))
                                                                  (:credit runner)))}
                                      :msg (msg "prevent " target " damage")
                                      :effect (effect (trash card {:cause :ability-cost})
                                                      (damage-prevent (first (:pre-damage (eventmap @state))) target)
                                                      (lose-credits target))}
                                     card nil))}]})})
