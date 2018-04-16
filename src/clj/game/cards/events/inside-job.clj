(in-ns 'game.core)

(declare run-event)

(def card-events-inside-job
  {"Inside Job"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})