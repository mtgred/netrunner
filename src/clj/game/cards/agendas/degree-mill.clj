(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-degree-mill
  {"Degree Mill"
   {:steal-cost-bonus (req [:shuffle-installed-to-stack 2])}})
