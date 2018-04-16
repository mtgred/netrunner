(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-franchise-city
  {"Franchise City"
   {:events {:access {:req (req (is-type? target "Agenda"))
                      :msg "add it to their score area as an agenda worth 1 agenda point"
                      :effect (effect (as-agenda :corp card 1))}}}})
