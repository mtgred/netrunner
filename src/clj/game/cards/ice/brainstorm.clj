(in-ns 'game.core)

(def card-definitions-ice-brainstorm
  {"Brainstorm"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand runner)) " subroutines")}]
    :subroutines [(do-brain-damage 1)]}})
