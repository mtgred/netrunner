(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-brahman
  {"Brahman"
   (auto-icebreaker ["All"]
                    {:implementation "Adding non-virus program to top of Stack is manual"
                     :abilities [(break-sub 1 2 "ICE")
                                 (strength-pump 2 1)]})})