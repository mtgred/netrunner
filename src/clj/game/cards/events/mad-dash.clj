(in-ns 'game.core)

(def card-definitions-events-mad-dash
  {"Mad Dash"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:agenda-stolen {:silent (req true)
                             :effect (effect (update! (assoc card :steal true)))}
             :run-ends {:effect (req (if (:steal card)
                                       (do (as-agenda state :runner (get-card state card) 1)
                                           (system-msg state :runner
                                                       (str "adds Mad Dash to their score area as an agenda worth 1 agenda point")))
                                       (do (system-msg state :runner
                                                       (str "suffers 1 meat damage from Mad Dash"))
                                                       (damage state side eid :meat 1 {:card card})))
                                     (unregister-events state side card))}}}})
