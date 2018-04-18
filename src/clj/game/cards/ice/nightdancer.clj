(in-ns 'game.core)

(def card-definitions-ice-nightdancer
  {"Nightdancer"
   {:subroutines [{:label "The Runner loses [Click], if able. You have an additional [Click] to spend during your next turn."
                   :msg "force the runner to lose a [Click], if able. Corp gains an additional [Click] to spend during their next turn"
                   :effect (req
                             (lose state :runner :click 1)
                             (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]}})
