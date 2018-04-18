(in-ns 'game.core)

(def card-definitions-resources-wasteland
  {"Wasteland"
   {:events {:runner-trash {:req (req (and (first-installed-trash-own? state :runner)
                                           (installed? target)
                                           (= (:side target) "Runner")))
                            :effect (effect (gain :credit 1))
                            :msg "gain 1 [Credits]"}}}})
