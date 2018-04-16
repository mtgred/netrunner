(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-nfr
  {"Nfr"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Place 1 power counter on Nfr"
                 :msg "place 1 power counter on it"
                 :ability-type :manual-state
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}
                (break-sub 1 1 "Barrier")]
    :strength-bonus (req (get-in card [:counter :power] 0))}})
