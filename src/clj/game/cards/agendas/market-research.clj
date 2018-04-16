(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-market-research
  {"Market Research"
   {:interactive (req true)
    :req (req tagged)
    :effect (effect (add-counter card :agenda 1)
                    (set-prop card :agendapoints 3))}})
