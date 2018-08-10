(in-ns 'game.cards.events)

(def card-definition-early-bird
  {"Early Bird"
   (run-event
    {:msg (msg "make a run on " target " and gain [Click]")}
    nil
    (effect (gain :click 1)))})
