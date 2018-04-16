(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-the-future-is-now
  {"The Future is Now"
   {:interactive (req true)
    :prompt "Choose a card to add to HQ"
    :choices (req (:deck corp))
    :msg (msg "add a card from R&D to HQ and shuffle R&D")
    :effect (effect (shuffle! :deck)
                    (move target :hand))}})
