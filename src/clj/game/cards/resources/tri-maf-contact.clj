(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-tri-maf-contact
  {"Tri-maf Contact"
   {:abilities [{:cost [:click 1] :msg "gain 2 [Credits]" :once :per-turn
                 :effect (effect (gain :credit 2))}]
    :trash-effect {:effect (effect (damage eid :meat 3 {:unboostable true :card card}))}}})
