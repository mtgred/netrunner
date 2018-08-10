(in-ns 'game.cards.assets)

(def card-definition-project-junebug
  {"Project Junebug"
   (advance-ambush 1 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "do " (* 2 (get-counters (get-card state card) :advancement)) " net damage")
                      :async true
                      :effect (effect (damage eid :net (* 2 (get-counters (get-card state card) :advancement))
                                              {:card card}))})})
