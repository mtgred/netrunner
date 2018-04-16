(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-faust
  {"Faust"
   {:abilities [{:label "Trash 1 card from Grip to break 1 subroutine"
                 :prompt "Select a card from your grip to trash for Faust"
                 :choices {:req in-hand?}
                 :msg (msg "trash " (:title target) " and break 1 subroutine")
                 :effect (effect (trash target {:unpreventable true}))}
                {:label "Trash 1 card from Grip to add 2 strength"
                 :prompt "Select a card from your grip to trash for Faust"
                 :choices {:req in-hand?}
                 :msg (msg "trash " (:title target) " and add 2 strength")
                 :effect (effect (trash target {:unpreventable true})
                                 (pump card 2))}]}})