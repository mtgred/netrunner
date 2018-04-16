(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-breach
  {"Breach"
   (central-breaker "Barrier"
                    (break-sub 2 3 "Barrier")
                    (strength-pump 2 4))})
