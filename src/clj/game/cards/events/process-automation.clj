(in-ns 'game.core)

(def card-definitions-events-process-automation
  {"Process Automation"
   {:msg "gain 2 [Credits] and draw 1 card"
    :effect (effect (gain :credit 2) (draw 1))}})
