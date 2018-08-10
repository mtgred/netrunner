(in-ns 'game.cards.events)

(def card-definition-calling-in-favors
  {"Calling in Favors"
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                     (all-active-installed state :runner))) " [Credits]")
    :effect (effect (gain-credits (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                                 (all-active-installed state :runner)))))}})
