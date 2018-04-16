(in-ns 'game.core)

(declare run-event)

(def card-events-account-siphon
  {"Account Siphon"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                         " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                         " [Credits] and take 2 tags")
                               :delayed-completion true
                               :effect (req (when-completed (tag-runner state :runner 2)
                                                            (do (gain state :runner :credit (* 2 (min 5 (:credit corp))))
                                                                (lose state :corp :credit (min 5 (:credit corp)))
                                                                (effect-completed state side eid))))}} card))}})