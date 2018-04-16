(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-the-black-file
  {"The Black File"
   {:msg "prevent the Corp from winning the game unless they are flatlined"
    :effect (req (swap! state assoc-in [:corp :cannot-win-on-points] true))
    :events {:runner-turn-begins
             {:effect (req (if (>= (get-in card [:counter :power] 0) 2)
                             (do (move state side (dissoc card :counter) :rfg)
                                 (swap! state update-in [:corp] dissoc :cannot-win-on-points)
                                 (system-msg state side "removes The Black File from the game")
                                 (gain-agenda-point state :corp 0))
                             (add-counter state side card :power 1)))}}
    :trash-effect {:effect (req (swap! state update-in [:corp] dissoc :cannot-win-on-points)
                                (gain-agenda-point state :corp 0))}
    :leave-play (req (swap! state update-in [:corp] dissoc :cannot-win-on-points)
                     (gain-agenda-point state :corp 0))}})
