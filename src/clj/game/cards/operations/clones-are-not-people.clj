(in-ns 'game.core)

(def card-definitions-operations-clones-are-not-people
  {"\"Clones are not People\""
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :effect (effect (as-agenda :corp card 1))}}}})
