(in-ns 'game.cards.hardware)

(def card-definition-brain-cage
  {"Brain Cage"
   {:in-play [:hand-size 3]
    :effect (effect (damage eid :brain 1 {:card card}))}})
