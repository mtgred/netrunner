(in-ns 'game.core)

(def card-definitions-upgrades-ash-2x3zb9cy
  {"Ash 2X3ZB9CY"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 4
                                      :effect (req (max-access state side 0)
                                                   (when-not (:replace-access (get-in @state [:run :run-effect]))
                                                     (let [ash card]
                                                       (swap! state update-in [:run :run-effect]
                                                              #(assoc % :replace-access
                                                                        {:mandatory true
                                                                         :effect (effect (handle-access [ash])) :card ash})))))
                                      :msg "prevent the Runner from accessing cards other than Ash 2X3ZB9CY"}}}}})
