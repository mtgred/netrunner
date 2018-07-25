(in-ns 'game.cards.hardware)

(def card-definition-window
  {"Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                 :effect (effect (move (last (:deck runner)) :hand))}]}})
