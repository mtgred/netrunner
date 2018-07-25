(in-ns 'game.cards.events)

(def card-definition-exclusive-party
  {"Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))
              " [Credits]")
    :effect (effect (draw) (gain-credits (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))))}})
