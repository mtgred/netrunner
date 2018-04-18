(in-ns 'game.core)

(def card-definitions-hardware-daredevil
  {"Daredevil"
   {:in-play [:memory 2]
    :events {:run-big {:once :per-turn
                       :req (req (first-event? state side :run-big))
                       :msg "draw two cards"
                       :effect (effect (draw 2))}}}})
