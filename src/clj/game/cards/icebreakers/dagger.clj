(in-ns 'game.core)

(def card-definitions-icebreakers-dagger
  {"Dagger"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 5)]})})
