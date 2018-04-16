(in-ns 'game.core)

(declare run-event)

(def card-events-exclusive-party
  {"Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))
              " [Credits]")
    :effect (effect (draw) (gain :credit (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))))}})
