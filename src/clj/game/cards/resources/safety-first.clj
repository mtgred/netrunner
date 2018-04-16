(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-safety-first
  {"Safety First"
   {:in-play [:hand-size-modification -2]
    :events {:runner-turn-ends {:req (req (< (count (:hand runner)) (hand-size state :runner)))
                                :msg (msg "draw a card")
                                :effect (effect (draw 1))}}}})