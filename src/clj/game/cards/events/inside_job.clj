(in-ns 'game.cards.events)

(def card-definition-inside-job
  {"Inside Job"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})
