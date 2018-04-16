(in-ns 'game.core)

(def card-operations-aggressive-negotiation
  {"Aggressive Negotiation"
   {:req (req (:scored-agenda corp-reg)) :prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :effect (final-effect (move target :hand) (shuffle! :deck))
    :msg "search R&D for a card and add it to HQ"}})