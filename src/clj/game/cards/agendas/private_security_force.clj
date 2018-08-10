(in-ns 'game.cards.agendas)

(def card-definition-private-security-force
  {"Private Security Force"
   {:abilities [{:req (req tagged)
                 :cost [:click 1]
                 :effect (effect (damage eid :meat 1 {:card card}))
                 :msg "do 1 meat damage"}]}})
