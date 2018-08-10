(in-ns 'game.cards.agendas)

(def card-definition-market-research
  {"Market Research"
   {:interactive (req true)
    :req (req tagged)
    :effect (effect (add-counter card :agenda 1)
                    (set-prop card :agendapoints 3))}})
