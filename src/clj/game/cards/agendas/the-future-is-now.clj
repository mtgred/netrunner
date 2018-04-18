(in-ns 'game.core)

(def card-definitions-agendas-the-future-is-now
  {"The Future is Now"
   {:interactive (req true)
    :prompt "Choose a card to add to HQ"
    :choices (req (:deck corp))
    :msg (msg "add a card from R&D to HQ and shuffle R&D")
    :effect (effect (shuffle! :deck)
                    (move target :hand))}})
