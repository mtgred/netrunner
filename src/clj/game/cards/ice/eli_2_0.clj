(in-ns 'game.cards.ice)

(def card-definition-eli-2-0
  {"Eli 2.0"
   {:subroutines [{:msg "draw 1 card" :effect (effect (draw))}
                  end-the-run]
    :runner-abilities [(runner-break [:click 2] 2)]}})
