(in-ns 'game.core)

(def card-definitions-icebreakers-switchblade
  {"Switchblade"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Stealth credit restriction not enforced"
                     :abilities [(break-sub 1 0 "Sentry")
                                 (strength-pump 1 7)]})})
