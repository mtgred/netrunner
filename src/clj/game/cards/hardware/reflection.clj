(in-ns 'game.core)

(def card-hardware-reflection
  {"Reflection"
   {:in-play [:memory 1 :link 1]
    :events {:jack-out {:msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))) " from HQ")}}}})