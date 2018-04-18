(in-ns 'game.core)

(def card-definitions-ice-errand-boy
  {"Errand Boy"
   {:subroutines [(gain-credits 1)
                  {:msg "draw 1 card" :effect (effect (draw))}]}})
