(in-ns 'game.cards.events)

(def card-definition-mad-dash
  {"Mad Dash"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:agenda-stolen {:silent (req true)
                             :effect (effect (update! (assoc card :steal true)))}
             :run-ends {:async true
                        :effect (req (if (:steal card)
                                       (wait-for (as-agenda state :runner (get-card state card) 1)
                                                 (system-msg state :runner
                                                             (str "adds Mad Dash to their score area as an agenda worth 1 agenda point")))
                                       (do (system-msg state :runner
                                                       (str "suffers 1 meat damage from Mad Dash"))
                                           (damage state side eid :meat 1 {:card card})))
                                     (unregister-events state side card))}}}})
