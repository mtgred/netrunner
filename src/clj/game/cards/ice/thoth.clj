(in-ns 'game.core)

(def card-definitions-ice-thoth
  {"Thoth"
   {:implementation "Encounter effect is manual"
    :runner-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Thoth")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Runner tag"
                                    :delayed-completion true
                                    :msg (msg "do " (:tag runner) " net damage")
                                    :effect (effect (damage eid :net (:tag runner) {:card card}))})
                  (trace-ability 4 {:label "Runner loses 1 [Credits] for each tag"
                                    :delayed-completion true
                                    :msg (msg "force the Runner to lose " (:tag runner) " [Credits]")
                                    :effect (effect (lose :runner :credit (:tag runner)))})]}})
