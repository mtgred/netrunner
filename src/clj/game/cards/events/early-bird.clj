(in-ns 'game.core)

(def card-definitions-events-early-bird
  {"Early Bird"
   (run-event
    {:msg (msg "make a run on " target " and gain [Click]")}
    nil
    (effect (gain :click 1)))})
