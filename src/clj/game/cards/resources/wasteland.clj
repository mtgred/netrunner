(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-wasteland
  {"Wasteland"
   {:events {:runner-trash {:req (req (and (first-installed-trash-own? state :runner)
                                           (installed? target)
                                           (= (:side target) "Runner")))
                            :effect (effect (gain :credit 1))
                            :msg "gain 1 [Credits]"}}}})