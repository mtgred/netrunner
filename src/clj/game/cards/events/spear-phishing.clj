(in-ns 'game.core)

(declare run-event)

(def card-events-spear-phishing
  {"Spear Phishing"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})