(in-ns 'game.core)

(declare run-event)

(def card-events-fisk-investment-seminar
  {"Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :corp 3))}})
