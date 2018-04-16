(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-obokata-protocol
  {"Obokata Protocol"
   {:steal-cost-bonus (req [:net-damage 4])}})