(in-ns 'game.core)

(def card-definitions-hardware-astrolabe
  {"Astrolabe"
   {:in-play [:memory 1]
    :events {:server-created {:msg "draw 1 card"
                              :effect (effect (draw :runner))}}}})
