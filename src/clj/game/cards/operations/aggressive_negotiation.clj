(in-ns 'game.cards.operations)

(def card-definition-aggressive-negotiation
  {"Aggressive Negotiation"
   {:req (req (:scored-agenda corp-reg)) :prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :effect (effect (move target :hand)
                    (shuffle! :deck))
    :msg "search R&D for a card and add it to HQ"}})
