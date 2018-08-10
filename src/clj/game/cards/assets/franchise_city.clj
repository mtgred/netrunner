(in-ns 'game.cards.assets)

(def card-definition-franchise-city
  {"Franchise City"
   {:events {:access {:req (req (is-type? target "Agenda"))
                      :msg "add it to their score area as an agenda worth 1 agenda point"
                      :async true
                      :effect (req (as-agenda state :corp eid card 1))}}}})
