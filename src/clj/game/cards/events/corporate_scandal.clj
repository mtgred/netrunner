(in-ns 'game.cards.events)

(def card-definition-corporate-scandal
  {"Corporate Scandal"
   {:msg "give the Corp 1 additional bad publicity"
    :implementation "No enforcement that this Bad Pub cannot be removed"
    :effect (req (swap! state update-in [:corp :has-bad-pub] inc))
    :leave-play (req (swap! state update-in [:corp :has-bad-pub] dec))}})
