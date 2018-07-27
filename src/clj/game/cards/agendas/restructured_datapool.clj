(in-ns 'game.cards.agendas)

(def card-definition-restructured-datapool
  {"Restructured Datapool"
   {:abilities [{:cost [:click 1]
                 :label "Tag the runner"
                 :trace {:base 2
                         :successful {:msg "give the runner 1 tag"
                                      :async true
                                      :effect (effect (gain-tags eid 1))}}}]}})
