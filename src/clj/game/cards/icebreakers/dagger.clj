(in-ns 'game.cards.icebreakers)

(def card-definition-dagger
  {"Dagger"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 5)]})})
