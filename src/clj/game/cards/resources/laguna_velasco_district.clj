(in-ns 'game.cards.resources)

(def card-definition-laguna-velasco-district
  {"Laguna Velasco District"
   {:events {:runner-click-draw {:msg "draw 1 card" :effect (effect (draw))}}}})
