(in-ns 'game.cards.assets)

(def card-definition-edge-of-world
  {"Edge of World"
   (letfn [(ice-count [state]
             (count (get-in (:corp @state) [:servers (last (:server (:run @state))) :ices])))]
     (installed-access-trigger 3 {:msg (msg "do " (ice-count state) " brain damage")
                                  :async true
                                  :effect (effect (damage eid :brain (ice-count state)
                                                          {:card card}))}))})
