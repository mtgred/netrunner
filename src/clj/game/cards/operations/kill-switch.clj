(in-ns 'game.core)

(def card-operations-kill-switch
  {"Kill Switch"
   (let [trace-for-brain-damage {:msg (msg "reveal that they accessed " (:title target))
                                 :trace {:base 3
                                         :msg "do 1 brain damage"
                                         :delayed-completion true
                                         :effect (effect (damage :runner eid :brain 1 {:card card}))}}]
     {:events {:pre-access-card (assoc trace-for-brain-damage :req (req (is-type? target "Agenda")))
               :agenda-scored trace-for-brain-damage}})})
