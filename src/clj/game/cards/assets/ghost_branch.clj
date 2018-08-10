(in-ns 'game.cards.assets)

(def card-definition-ghost-branch
  {"Ghost Branch"
   (advance-ambush 0 {:async true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "give the Runner " (quantify (get-counters (get-card state card) :advancement) "tag"))
                      :effect (effect (gain-tags :corp eid (get-counters (get-card state card) :advancement)))})})
