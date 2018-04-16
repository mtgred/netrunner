(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-echo-chamber
  {"Echo Chamber"
   {:abilities [{:label "Add Echo Chamber to your score area as an agenda worth 1 agenda point"
                 :cost [:click 3]
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :effect (effect (as-agenda :corp card 1)) }]}})