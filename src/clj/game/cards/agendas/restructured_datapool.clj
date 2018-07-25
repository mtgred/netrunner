(in-ns 'game.cards.agendas)

(def card-definition-restructured-datapool
  {"Restructured Datapool"
   {:abilities [{:cost [:click 1]
                 :trace {:base 2
                         :successful {:msg "give the Runner 1 tag"
                                      :async true
                                      :effect (effect (gain-tags eid 1))}}}]}})
