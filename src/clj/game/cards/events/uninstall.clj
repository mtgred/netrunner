(in-ns 'game.core)

(def card-definitions-events-uninstall
  {"Uninstall"
   {:choices {:req #(and (installed? %)
                         (not (facedown? %))
                         (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}})
