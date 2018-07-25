(in-ns 'game.cards.agendas)

(def card-definition-net-quarantine
  {"Net Quarantine"
   (let [nq {:effect (req (let [extra (int (/ (:runner-spent target) 2))]
                            (when (pos? extra)
                              (gain-credits state side extra)
                              (system-msg state :corp (str "uses Net Quarantine to gain " extra "[Credits]")))))}]
     {:events {:pre-init-trace {:once :per-turn
                                :silent (req true)
                                :effect (req (system-msg state :corp "uses Net Quarantine to reduce Runner's base link to zero")
                                             (swap! state assoc-in [:trace :force-link] 0))}
               :successful-trace nq
               :unsuccessful-trace nq}})})
