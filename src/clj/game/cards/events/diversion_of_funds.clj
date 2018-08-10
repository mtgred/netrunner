(in-ns 'game.cards.events)

(def card-definition-diversion-of-funds
  {"Diversion of Funds"
   {:req (req hq-runnable)
    :effect (effect (run :hq
                         {:req (req (= target :hq))
                          :replace-access
                          (let [five-or-all (fn [corp] (min 5 (:credit corp)))]
                            {:msg (msg "force the Corp to lose " (five-or-all corp)
                                       "[Credits], and gain " (five-or-all corp) "[Credits]")
                             :effect (effect (lose-credits :corp (five-or-all corp))
                                             (gain-credits :runner (five-or-all corp)))})}
                      card))}})
