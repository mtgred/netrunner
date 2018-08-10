(in-ns 'game.cards.hardware)

(def card-definition-zer0
  {"Zer0"
   {:abilities [{:cost [:click 1 :net-damage 1]
                 :once :per-turn
                 :msg "gain 1 [Credits] and draw 2 cards"
                 :effect (effect (gain-credits 1)
                                 (draw 2))}]}})
