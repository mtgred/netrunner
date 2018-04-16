(in-ns 'game.core)

(declare run-event)

(def card-events-paper-tripping
  {"Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose :tag :all))}})