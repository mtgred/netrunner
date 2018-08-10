(in-ns 'game.cards.events)

(def card-definition-mars-for-martians
  {"Mars for Martians"
   {:msg (msg "draw " (count (filter #(and (has-subtype? % "Clan") (is-type? % "Resource"))
                                     (all-active-installed state :runner)))
              " cards and gain " (:tag runner) " [Credits]")
    :effect (effect (draw (count (filter #(and (has-subtype? % "Clan") (is-type? % "Resource"))
                                         (all-active-installed state :runner))))
                    (gain-credits (:tag runner)))}})
