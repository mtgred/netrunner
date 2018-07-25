(in-ns 'game.cards.assets)

(def card-definition-breached-dome
  {"Breached Dome"
   {:flags {:rd-reveal (req true)}
    :access {:async true
             :effect (req (let [c (first (get-in @state [:runner :deck]))]
                            (system-msg state :corp (str "uses Breached Dome to do one meat damage and to trash " (:title c)
                                                         " from the top of the Runner's Stack"))
                            (mill state :corp :runner 1)
                            (damage state side eid :meat 1 {:card card})))}}})
