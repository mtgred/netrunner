(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-passport
  {"Passport"
   (central-breaker "Code Gate"
                    (break-sub 1 1 "Code Gate")
                    (strength-pump 2 2))})