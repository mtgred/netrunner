(in-ns 'game.cards.operations)

(def card-definition-kill-switch
  {"Kill Switch"
   (let [trace-for-brain-damage {:msg (msg "reveal that they accessed " (:title target))
                                 :trace {:base 3
                                         :successful {:msg "do 1 brain damage"
                                                      :async true
                                                      :effect (effect (damage :runner eid :brain 1 {:card card}))}}}]
     {:events {:access (assoc trace-for-brain-damage :req (req (is-type? target "Agenda"))
                                                     :interactive (req (is-type? target "Agenda")))
               :agenda-scored trace-for-brain-damage}})})
