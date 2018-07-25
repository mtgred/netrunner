(in-ns 'game.cards.hardware)

(def card-definition-sports-hopper
  {"Sports Hopper"
   {:in-play [:link 1]
    :abilities [{:label "Draw 3 cards"
                 :msg "draw 3 cards"
                 :effect (effect (trash card {:cause :ability-cost}) (draw 3))}]}})
