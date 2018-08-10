(in-ns 'game.cards.programs)

(def card-definition-reaver
  {"Reaver"
   {:events {:runner-trash {:req (req (and (first-installed-trash? state side)
                                           (installed? target)))
                            :effect (effect (draw :runner 1))
                            :msg "draw 1 card"}}}})
