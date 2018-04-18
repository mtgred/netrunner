(in-ns 'game.core)

(def card-definitions-agendas-show-of-force
  {"Show of Force"
   {:delayed-completion true
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))}})
