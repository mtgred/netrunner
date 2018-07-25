(in-ns 'game.cards.hardware)

(def card-definition-astrolabe
  {"Astrolabe"
   {:in-play [:memory 1]
    :events {:server-created {:msg "draw 1 card"
                              :async true
                              :effect (effect (draw :runner eid 1 nil))}}}})
