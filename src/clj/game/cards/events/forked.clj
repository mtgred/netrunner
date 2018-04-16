(in-ns 'game.core)

(declare run-event)

(def card-events-forked
  {"Forked"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})
