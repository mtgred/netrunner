(in-ns 'game.cards.ice)

(def card-definition-errand-boy
  {"Errand Boy"
   {:subroutines [(gain-credits-sub 1)
                  {:msg "draw 1 card" :effect (effect (draw))}]}})
