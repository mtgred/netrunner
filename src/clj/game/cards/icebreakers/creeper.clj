(in-ns 'game.core)

(def card-definitions-icebreakers-creeper
  {"Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "Sentry")
                                   (strength-pump 1 1)]}))})
