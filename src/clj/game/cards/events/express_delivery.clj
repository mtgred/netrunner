(in-ns 'game.cards.events)

(def card-definition-express-delivery
  {"Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}})
