(in-ns 'game.core)

(def card-definitions-assets-cerebral-overwriter
  {"Cerebral Overwriter"
   (advance-ambush 3 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :msg (msg "do " (:advance-counter (get-card state card) 0) " brain damage")
                      :delayed-completion true
                      :effect (effect (damage eid :brain (:advance-counter (get-card state card) 0) {:card card}))})})
