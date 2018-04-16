(in-ns 'game.core)

(declare run-event)

(def card-events-lawyer-up
  {"Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose :tag 2))}})