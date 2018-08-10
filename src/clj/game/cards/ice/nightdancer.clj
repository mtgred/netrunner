(in-ns 'game.cards.ice)

(def card-definition-nightdancer
  {"Nightdancer"
   {:subroutines [{:label (str "The Runner loses [Click], if able. "
                               "You have an additional [Click] to spend during your next turn.")
                   :msg (str "force the runner to lose a [Click], if able. "
                             "Corp gains an additional [Click] to spend during their next turn")
                   :effect (req (lose state :runner :click 1)
                                (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]}})
