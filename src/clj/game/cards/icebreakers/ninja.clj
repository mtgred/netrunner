(in-ns 'game.core)

(def card-definitions-icebreakers-ninja
  {"Ninja"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)]})})
