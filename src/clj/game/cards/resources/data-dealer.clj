(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-data-dealer
  {"Data Dealer"
   {:abilities [{:cost [:click 1 :forfeit] :effect (effect (gain :credit 9))
                 :msg (msg "gain 9 [Credits]")}]}})