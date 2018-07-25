(in-ns 'game.cards.identities)

(def card-definition-liza-talking-thunder-prominent-legislator
  {"Liza Talking Thunder: Prominent Legislator"
   {:events
    {:successful-run
     {:async true
      :interactive (req true)
      :msg "draw 2 cards and take 1 tag"
      :req (req (and (is-central? (:server run))
                     (first-event? state side :successful-run is-central?)))
      :effect (req (wait-for (gain-tags state :runner 1)
                             (draw state :runner eid 2 nil)))}}}})
