(in-ns 'game.core)

(def card-definitions-operations-manhunt
  {"Manhunt"
   {:events {:successful-run {:interactive (req true)
                              :req (req (first-event? state side :successful-run))
                              :trace {:base 2 :msg "give the Runner 1 tag"
                                      :delayed-completion true
                                      :effect (effect (tag-runner :runner eid 1))}}}}})
