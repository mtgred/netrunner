(in-ns 'game.core)

(def card-definitions-agendas-net-quarantine
  {"Net Quarantine"
   (let [nq {:effect (req (let [extra (int (/ (:runner-spent target) 2))]
                            (when (pos? extra)
                              (gain state side :credit extra)
                              (system-msg state :corp (str "uses Net Quarantine to gain " extra " [Credits]")))
                            (when (some? (get-in @state [:runner :temp-link]))
                              (swap! state assoc-in [:runner :link] (:temp-link runner))
                              (swap! state dissoc-in [:runner :temp-link]))))}]
   {:events {:trace {:once :per-turn
                     :silent (req true)
                     :effect (req (system-msg state :corp "uses Net Quarantine to reduce Runner's base link to zero")
                                  (swap! state assoc-in [:runner :temp-link] (:link runner))
                                  (swap! state assoc-in [:runner :link] 0))}
             :successful-trace nq
             :unsuccessful-trace nq}})})
