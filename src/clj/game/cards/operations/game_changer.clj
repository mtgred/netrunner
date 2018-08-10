(in-ns 'game.cards.operations)

(def card-definition-game-changer
  {"Game Changer"
   {:effect (req (gain state side :click (count (:scored runner)))
                 (move state side (first (:play-area corp)) :rfg))}})
