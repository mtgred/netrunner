(in-ns 'game.cards.assets)

(def card-definition-cerebral-overwriter
  {"Cerebral Overwriter"
   (advance-ambush 3 {:async true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "do " (get-counters (get-card state card) :advancement) " brain damage")
                      :effect (effect (damage eid :brain (get-counters (get-card state card) :advancement) {:card card}))})})
