(in-ns 'game.core)

(def card-definitions-events-freedom-through-equality
  {"\"Freedom Through Equality\""
   {:events {:agenda-stolen {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :effect (effect (as-agenda :runner card 1))}}}})
