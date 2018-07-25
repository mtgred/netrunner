(in-ns 'game.cards.icebreakers)

(def card-definition-creeper
  {"Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "Sentry")
                                   (strength-pump 1 1)]}))})
