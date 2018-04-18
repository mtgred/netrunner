(in-ns 'game.core)

(def card-definitions-resources-gene-conditioning-shoppe
  {"Gene Conditioning Shoppe"
   {:msg "make Genetics trigger a second time each turn"
    :effect (effect (register-persistent-flag! card :genetics-trigger-twice (constantly true)))
    :leave-play (effect (clear-persistent-flag! card :genetics-trigger-twice))}})
