(in-ns 'game.cards.events)

(def card-definition-account-siphon
  {"Account Siphon"
   {:req (req hq-runnable)
    :makes-run true
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                         " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                         " [Credits] and take 2 tags")
                               :async true
                               :effect (req (wait-for (gain-tags state :runner 2)
                                                      (do (gain-credits state :runner (* 2 (min 5 (:credit corp))))
                                                          (lose-credits state :corp (min 5 (:credit corp)))
                                                          (effect-completed state side eid))))}}
                         card))}})
