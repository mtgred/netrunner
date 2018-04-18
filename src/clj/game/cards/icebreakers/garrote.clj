(in-ns 'game.core)

(def card-definitions-icebreakers-garrote
  {"Garrote"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 1)]})})
