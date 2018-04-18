(in-ns 'game.core)

(def card-definitions-events-lawyer-up
  {"Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose :tag 2))}})
