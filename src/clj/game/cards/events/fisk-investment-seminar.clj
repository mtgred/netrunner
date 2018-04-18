(in-ns 'game.core)

(def card-definitions-events-fisk-investment-seminar
  {"Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :corp 3))}})
