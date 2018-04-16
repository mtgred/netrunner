(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-black-orchestra
  {"Black Orchestra"
   (conspiracy "Black Orchestra" "Code Gate"
               [{:cost [:credit 3]
                 :effect (effect (pump card 2)) :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}])})
