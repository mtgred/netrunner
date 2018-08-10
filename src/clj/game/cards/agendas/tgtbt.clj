(in-ns 'game.cards.agendas)

(def card-definition-tgtbt
  {"TGTBT"
   {:flags {:rd-reveal (req true)}
    :access {:msg "give the Runner 1 tag"
             :async true
             :effect (effect (gain-tags eid 1))}}})
