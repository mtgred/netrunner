(in-ns 'game.cards.icebreakers)

(def card-definition-mongoose
  {"Mongoose"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Usage restriction is not implemented"
                     :abilities [(break-sub 1 2 "Sentry")
                                 (strength-pump 2 2)]})})
