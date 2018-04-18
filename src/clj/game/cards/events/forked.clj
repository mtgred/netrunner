(in-ns 'game.core)

(def card-definitions-events-forked
  {"Forked"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})
