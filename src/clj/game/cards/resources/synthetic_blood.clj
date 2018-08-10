(in-ns 'game.cards.resources)

(def card-definition-synthetic-blood
  {"Synthetic Blood"
   {:events {:damage {:req (req (genetics-trigger? state side :damage))
                      :msg "draw 1 card"
                      :effect (effect (draw :runner))}}}})
