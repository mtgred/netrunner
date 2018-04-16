(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-show-of-force
  {"Show of Force"
   {:delayed-completion true
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))}})