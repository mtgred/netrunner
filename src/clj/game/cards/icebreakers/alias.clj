(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-alias
  {"Alias"
   (central-breaker "Sentry"
                    (break-sub 1 1 "Sentry")
                    (strength-pump 2 3))})
