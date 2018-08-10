(in-ns 'game.cards.assets)

(def card-definition-echo-chamber
  {"Echo Chamber"
   {:abilities [{:label "Add Echo Chamber to your score area as an agenda worth 1 agenda point"
                 :cost [:click 3]
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :async true
                 :effect (req (as-agenda state :corp eid card 1))}]}})
