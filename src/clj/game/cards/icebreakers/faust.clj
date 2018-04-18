(in-ns 'game.core)

(def card-definitions-icebreakers-faust
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
