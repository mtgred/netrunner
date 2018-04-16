(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-fetal-ai
  {"Fetal AI"
   {:flags {:rd-reveal (req true)}
    :access {:delayed-completion true
             :req (req (not= (first (:zone card)) :discard)) :msg "do 2 net damage"
             :effect (effect (damage eid :net 2 {:card card}))}
    :steal-cost-bonus (req [:credit 2])}})