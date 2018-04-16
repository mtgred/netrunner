(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-private-security-force
  {"Private Security Force"
   {:abilities [{:req (req tagged)
                 :cost [:click 1]
                 :effect (effect (damage eid :meat 1 {:card card}))
                 :msg "do 1 meat damage"}]}})
