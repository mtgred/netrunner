(in-ns 'game.cards.icebreakers)

(def card-definition-pipeline
  {"Pipeline"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1 :all-run)]})})
