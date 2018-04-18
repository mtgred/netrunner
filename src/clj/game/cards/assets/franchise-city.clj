(in-ns 'game.core)

(def card-definitions-assets-franchise-city
  {"Franchise City"
   {:events {:access {:req (req (is-type? target "Agenda"))
                      :msg "add it to their score area as an agenda worth 1 agenda point"
                      :effect (effect (as-agenda :corp card 1))}}}})
