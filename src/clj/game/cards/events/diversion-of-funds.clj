(in-ns 'game.core)

(declare run-event)

(def card-events-diversion-of-funds
  {"Diversion of Funds"
   {:req (req hq-runnable)
    :effect (effect (run :hq
                         {:req (req (= target :hq))
                          :replace-access
                          (let [five-or-all (fn [corp] (min 5 (:credit corp)))]
                            {:delayed-completion true
                             :msg (msg "force the Corp to lose " (five-or-all corp)
                                       " [Credits], and gain " (five-or-all corp))
                             :effect (effect (lose :corp :credit (five-or-all corp))
                                             (gain :runner :credit (five-or-all corp)))})}
                      card))}})
