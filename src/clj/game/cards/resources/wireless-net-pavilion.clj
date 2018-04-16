(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-wireless-net-pavilion
  {"Wireless Net Pavilion"
   {:effect (effect (trash-resource-bonus -2))
    :leave-play (effect (trash-resource-bonus 2))}})