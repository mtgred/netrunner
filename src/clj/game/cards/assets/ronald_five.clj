(in-ns 'game.cards.assets)

(def card-definition-ronald-five
  {"Ronald Five"
   {:events {:runner-trash {:req (req (and (= (:side target) "Corp")
                                           (pos? (:click runner))))
                            :msg "force the runner to lose 1 [Click]"
                            :effect (effect (lose :runner :click 1))}}}})
