(in-ns 'game.core)

(def card-definitions-events-calling-in-favors
  {"Calling in Favors"
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                     (all-active-installed state :runner))) " [Credits]")
    :effect (effect (gain :credit (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                                 (all-active-installed state :runner)))))}})
