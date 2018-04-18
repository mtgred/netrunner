(in-ns 'game.core)

(def card-definitions-assets-jackson-howard
  {"Jackson Howard"
   {:abilities [{:cost [:click 1] :effect (effect (draw 2)) :msg "draw 2 cards"}
                {:label "Shuffle up to 3 cards from Archives into R&D"
                 :activatemsg "removes Jackson Howard from the game"
                 :effect (effect (rfg-and-shuffle-rd-effect card 3))}]}})
