(in-ns 'game.cards.resources)

(def card-definition-fan-site
  {"Fan Site"
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 0 agenda points"
                             :async true
                             :req (req (installed? card))
                             :effect (req (as-agenda state :runner eid card 0))}}}})
