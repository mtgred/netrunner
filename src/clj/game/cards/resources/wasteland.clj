(in-ns 'game.cards.resources)

(def card-definition-wasteland
  {"Wasteland"
   {:events {:runner-trash {:req (req (and (first-installed-trash-own? state :runner)
                                           (installed? target)
                                           (= (:side target) "Runner")))
                            :effect (effect (gain-credits 1))
                            :msg "gain 1 [Credits]"}}}})
