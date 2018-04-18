(in-ns 'game.core)

(def card-definitions-assets-the-news-now-hour
  {"The News Now Hour"
   {:events {:runner-turn-begins {:effect (req (prevent-current state side))}}
    :effect (req (prevent-current state side))
    :leave-play (req (swap! state assoc-in [:runner :register :cannot-play-current] false))}})
