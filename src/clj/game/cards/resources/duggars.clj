(in-ns 'game.core)

(def card-definitions-resources-duggars
  {"Duggars"
   {:abilities [{:cost [:click 4] :effect (effect (draw 10)) :msg "draw 10 cards"}]}})
