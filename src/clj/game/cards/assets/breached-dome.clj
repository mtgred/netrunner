(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-breached-dome
  {"Breached Dome"
   {:flags {:rd-reveal (req true)}
    :access {:delayed-completion true
             :effect (req (let [c (first (get-in @state [:runner :deck]))]
                            (system-msg state :corp (str "uses Breached Dome to do one meat damage and to trash " (:title c)
                                                         " from the top of the Runner's Stack"))
                            (mill state :corp :runner 1)
                            (damage state side eid :meat 1 {:card card})))}}})
