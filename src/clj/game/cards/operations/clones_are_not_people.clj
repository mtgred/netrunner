(in-ns 'game.cards.operations)

(def card-definition-clones-are-not-people
  {"\"Clones are not People\""
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :async true
                             :effect (req (as-agenda state :corp eid card 1))}}}})
