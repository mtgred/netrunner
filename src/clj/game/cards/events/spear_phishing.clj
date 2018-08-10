(in-ns 'game.cards.events)

(def card-definition-spear-phishing
  {"Spear Phishing"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}})
