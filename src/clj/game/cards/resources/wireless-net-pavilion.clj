(in-ns 'game.core)

(def card-definitions-resources-wireless-net-pavilion
  {"Wireless Net Pavilion"
   {:effect (effect (trash-resource-bonus -2))
    :leave-play (effect (trash-resource-bonus 2))}})
