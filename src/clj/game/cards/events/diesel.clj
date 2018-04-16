(in-ns 'game.core)

(declare run-event)

(def card-events-diesel
  {"Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}})