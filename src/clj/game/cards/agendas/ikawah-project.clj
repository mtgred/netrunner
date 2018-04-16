(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-ikawah-project
  {"Ikawah Project"
   {:steal-cost-bonus (req [:credit 2 :click 1])}})