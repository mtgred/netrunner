(in-ns 'game.core)

(declare run-event)

(def card-events-uninstall
  {"Uninstall"
   {:choices {:req #(and (installed? %)
                         (not (facedown? %))
                         (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}})