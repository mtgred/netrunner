(in-ns 'game.cards.assets)

(def card-definition-jackson-howard
  {"Jackson Howard"
   {:abilities [{:cost [:click 1]
                 :msg "draw 2 cards"
                 :effect (effect (draw 2))}
                {:label "Shuffle up to 3 cards from Archives into R&D"
                 :activatemsg "removes Jackson Howard from the game"
                 :effect (effect (rfg-and-shuffle-rd-effect card 3))}]}})
