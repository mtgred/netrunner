(in-ns 'game.cards.events)

(def card-definition-hot-pursuit
  {"Hot Pursuit"
   {:req (req hq-runnable)
    :makes-run true
    :effect (effect (run :hq {:req (req (= target :hq))
                              :successful-run {:async true
                                               :msg "gain 9 [Credits] and take 1 tag"
                                               :effect (req (wait-for (gain-tags state :runner 1)
                                                                      (gain-credits state :runner 9)
                                                                      (effect-completed state side eid)))}} card))}})
