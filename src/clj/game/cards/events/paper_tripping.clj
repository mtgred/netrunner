(in-ns 'game.cards.events)

(def card-definition-paper-tripping
  {"Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose-tags :all))}})
