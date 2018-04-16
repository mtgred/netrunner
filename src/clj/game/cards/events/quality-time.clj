(in-ns 'game.core)

(declare run-event)

(def card-events-quality-time
  {"Quality Time"
   {:msg "draw 5 cards" :effect (effect (draw 5))}})
