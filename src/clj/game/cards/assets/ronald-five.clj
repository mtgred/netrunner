(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-ronald-five
  {"Ronald Five"
   {:events {:runner-trash {:req (req (and (= (:side target) "Corp") (> (:click runner) 0)))
                            :msg "force the runner to lose 1 [Click]" :effect (effect (lose :runner :click 1))}}}})
