(in-ns 'game.core)

(def card-definitions-events-diesel
  {"Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}})
