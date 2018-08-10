(in-ns 'game.cards.events)

(def card-definition-freedom-through-equality
  {"\"Freedom Through Equality\""
   {:events {:agenda-stolen {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :async true
                             :effect (req (as-agenda state :runner eid card 1))}}}})
