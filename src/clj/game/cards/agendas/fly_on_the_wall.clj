(in-ns 'game.cards.agendas)

(def card-definition-fly-on-the-wall
  {"Fly on the Wall"
   {:msg "give the runner 1 tag"
    :async true
    :effect (req (gain-tags state :runner eid 1))}})
