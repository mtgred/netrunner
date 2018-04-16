(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-genetics-pavilion
  {"Genetics Pavilion"
   {:msg "prevent the Runner from drawing more than 2 cards during their turn"
    :effect (req (max-draw state :runner 2)
                 (when (= 0 (remaining-draws state :runner))
                   (prevent-draw state :runner)))
    :events {:runner-turn-begins {:effect (effect (max-draw :runner 2))}}
    :leave-play (req (swap! state update-in [:runner :register] dissoc :max-draw :cannot-draw))}})