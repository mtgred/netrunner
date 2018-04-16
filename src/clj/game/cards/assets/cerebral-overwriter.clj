(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-cerebral-overwriter
  {"Cerebral Overwriter"
   (advance-ambush 3 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :msg (msg "do " (:advance-counter (get-card state card) 0) " brain damage")
                      :delayed-completion true
                      :effect (effect (damage eid :brain (:advance-counter (get-card state card) 0) {:card card}))})})
