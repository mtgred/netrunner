(in-ns 'game.core)

(declare run-event)

(def card-events-game-day
  {"Game Day"
   {:msg (msg "draw " (- (hand-size state :runner) (count (:hand runner))) " cards")
    :effect (effect (draw (- (hand-size state :runner) (count (:hand runner)))))}})
