(in-ns 'game.cards.ice)

(def card-definition-data-raven
  {"Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)
                (power-counter-ability (give-tags 1))]
    :runner-abilities [{:label "End the run"
                        :effect (req (end-run state :runner)
                                     (system-msg state :runner "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Raven")
                                     (gain-tags state :runner eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}})
