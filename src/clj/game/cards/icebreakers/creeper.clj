(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-creeper
  {"Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "Sentry")
                                   (strength-pump 1 1)]}))})