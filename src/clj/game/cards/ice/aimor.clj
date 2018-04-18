(in-ns 'game.core)

(def card-definitions-ice-aimor
  {"Aimor"
   {:subroutines [{:label "Trash the top 3 cards of the Stack. Trash Aimor."
                   :effect (req (when (not-empty (:deck runner))
                                  (system-msg state :corp
                                              (str "uses Aimor to trash "
                                                   (join ", " (map :title (take 3 (:deck runner))))
                                                   " from the Runner's Stack"))
                                  (mill state :corp :runner 3))
                                (when current-ice
                                  (no-action state :corp nil)
                                  (continue state :runner nil))
                                (trash state side card)
                                (system-msg state side (str "trashes Aimor")))}]}})
