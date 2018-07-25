(in-ns 'game.cards.icebreakers)

(def card-definition-nfr
  {"Nfr"
   {:implementation "Adding power counter is manual"
    :abilities [{:label "Place 1 power counter on Nfr"
                 :msg "place 1 power counter on it"
                 :ability-type :manual-state
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}
                (break-sub 1 1 "Barrier")]
    :strength-bonus (req (get-counters card :power))}})
