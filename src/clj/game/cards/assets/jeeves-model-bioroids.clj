(in-ns 'game.core)

(def card-definitions-assets-jeeves-model-bioroids
  {"Jeeves Model Bioroids"
   (let [jeeves (effect (gain :click 1))
         ability {:label "Gain [Click]"
                  :msg "gain [Click]"
                  :once :per-turn
                  :effect jeeves}
         cleanup (effect (update! (dissoc card :seen-this-turn)))]
     {:abilities [ability]
      :leave-play cleanup
      :trash-effect {:effect cleanup}
      :events {:corp-spent-click
               {:effect (req (when-not target
                               (print-stack-trace (Exception. (str "WHY JEEVES WHY: " targets))))
                             (update! state side (update-in card [:seen-this-turn (or target :this-is-a-hack)]
                                                            (fnil + 0) (second targets)))
                             (when (>= (get-in (get-card state card) [:seen-this-turn (or target :this-is-a-hack)]) 3)
                               (resolve-ability state side ability card nil)))}
               :corp-turn-ends {:effect cleanup}}})})
