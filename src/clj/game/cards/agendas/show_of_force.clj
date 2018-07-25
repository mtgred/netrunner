(in-ns 'game.cards.agendas)

(def card-definition-show-of-force
  {"Show of Force"
   {:async true
    :msg "do 2 meat damage"
    :effect (effect (damage eid :meat 2 {:card card}))}})
