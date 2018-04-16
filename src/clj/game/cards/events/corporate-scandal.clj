(in-ns 'game.core)

(declare run-event)

(def card-events-corporate-scandal
  {"Corporate Scandal"
   {:msg "give the Corp 1 additional bad publicity"
    :implementation "No enforcement that this Bad Pub cannot be removed"
    :effect (req (swap! state update-in [:corp :has-bad-pub] inc))
    :leave-play (req (swap! state update-in [:corp :has-bad-pub] dec))}})
