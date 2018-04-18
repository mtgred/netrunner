(in-ns 'game.core)

(def card-definitions-ice-data-raven
  {"Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag
                (power-counter-ability give-tag)]
    :runner-abilities [{:label "End the run"
                        :effect (req (end-run state :runner)
                                     (system-msg state :runner "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Raven")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}})
