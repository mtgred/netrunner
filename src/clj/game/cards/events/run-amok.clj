(in-ns 'game.core)

(declare run-event)

(def card-events-run-amok
  {"Run Amok"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target {:end-run {:msg " trash 1 piece of ICE that was rezzed during the run"}} card))}})