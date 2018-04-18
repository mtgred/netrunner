(in-ns 'game.core)

(def card-definitions-events-populist-rally
  {"Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-active-installed state :runner))))
    :msg "give the Corp 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :corp :click-per-turn 1)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-turn-ends {:effect (effect (gain :corp :click-per-turn 1)
                                              (unregister-events card))}}}})
