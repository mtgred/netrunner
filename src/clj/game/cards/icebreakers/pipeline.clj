(in-ns 'game.core)

(def card-definitions-icebreakers-pipeline
  {"Pipeline"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1 :all-run)]})})
