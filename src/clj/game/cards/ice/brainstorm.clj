(in-ns 'game.cards.ice)

(def card-definition-brainstorm
  {"Brainstorm"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand runner)) " subroutines")}]
    :subroutines [(do-brain-damage 1)]}})
