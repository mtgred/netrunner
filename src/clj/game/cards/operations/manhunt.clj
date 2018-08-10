(in-ns 'game.cards.operations)

(def card-definition-manhunt
  {"Manhunt"
   {:events {:successful-run {:interactive (req true)
                              :req (req (first-event? state side :successful-run))
                              :trace {:base 2
                                      :successful {:msg "give the Runner 1 tag"
                                                   :async true
                                                   :effect (effect (gain-tags eid 1))}}}}}})
