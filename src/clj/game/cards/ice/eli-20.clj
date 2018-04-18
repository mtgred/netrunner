(in-ns 'game.core)

(def card-definitions-ice-eli-20
  {"Eli 2.0"
   {:subroutines [{:msg "draw 1 card" :effect (effect (draw))}
                  end-the-run]
    :runner-abilities [(runner-break [:click 2] 2)]}})
