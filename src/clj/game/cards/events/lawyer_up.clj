(in-ns 'game.cards.events)

(def card-definition-lawyer-up
  {"Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose-tags 2))}})
