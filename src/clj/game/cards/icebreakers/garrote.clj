(in-ns 'game.cards.icebreakers)

(def card-definition-garrote
  {"Garrote"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 1)]})})
