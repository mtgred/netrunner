(in-ns 'game.cards.icebreakers)

(def card-definition-ninja
  {"Ninja"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)]})})
