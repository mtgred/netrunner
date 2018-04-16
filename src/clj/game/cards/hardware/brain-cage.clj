(in-ns 'game.core)

(def card-hardware-brain-cage
  {"Brain Cage"
   {:in-play [:hand-size-modification 3]
    :effect (effect (damage eid :brain 1 {:card card}))}})
