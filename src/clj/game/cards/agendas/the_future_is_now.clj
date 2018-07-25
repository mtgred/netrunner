(in-ns 'game.cards.agendas)

(def card-definition-the-future-is-now
  {"The Future is Now"
   {:interactive (req true)
    :prompt "Choose a card to add to HQ"
    :choices (req (:deck corp))
    :msg (msg "add a card from R&D to HQ and shuffle R&D")
    :req (req (pos? (count (:deck corp))))
    :effect (effect (shuffle! :deck)
                    (move target :hand))}})
