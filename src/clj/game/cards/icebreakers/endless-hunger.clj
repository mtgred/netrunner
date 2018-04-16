(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-endless-hunger
  {"Endless Hunger"
   {:abilities [{:label "Trash 1 installed card to break 1 \"End the run.\" subroutine"
                 :prompt "Select a card to trash for Endless Hunger"
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :msg (msg "trash " (:title target)
                           " and break 1 \"[Subroutine] End the run.\" subroutine")
                 :effect (effect (trash target {:unpreventable true}))}]}})
