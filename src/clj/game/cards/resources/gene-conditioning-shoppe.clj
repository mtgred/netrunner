(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-gene-conditioning-shoppe
  {"Gene Conditioning Shoppe"
   {:msg "make Genetics trigger a second time each turn"
    :effect (effect (register-persistent-flag! card :genetics-trigger-twice (constantly true)))
    :leave-play (effect (clear-persistent-flag! card :genetics-trigger-twice))}})