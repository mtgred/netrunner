(in-ns 'game.cards.resources)

(def card-definition-wireless-net-pavilion
  {"Wireless Net Pavilion"
   {:effect (effect (trash-resource-bonus -2))
    :leave-play (effect (trash-resource-bonus 2))}})
