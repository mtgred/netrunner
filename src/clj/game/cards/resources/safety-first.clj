(in-ns 'game.core)

(def card-definitions-resources-safety-first
  {"Safety First"
   {:in-play [:hand-size-modification -2]
    :events {:runner-turn-ends {:req (req (< (count (:hand runner)) (hand-size state :runner)))
                                :msg (msg "draw a card")
                                :effect (effect (draw 1))}}}})
