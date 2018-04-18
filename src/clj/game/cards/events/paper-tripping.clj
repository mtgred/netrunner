(in-ns 'game.core)

(def card-definitions-events-paper-tripping
  {"Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose :tag :all))}})
