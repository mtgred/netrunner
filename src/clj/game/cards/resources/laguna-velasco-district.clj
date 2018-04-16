(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-laguna-velasco-district
  {"Laguna Velasco District"
   {:events {:runner-click-draw {:msg "draw 1 card" :effect (effect (draw))}}}})
