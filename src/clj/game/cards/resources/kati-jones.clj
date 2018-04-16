(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-kati-jones
  {"Kati Jones"
   {:abilities [{:cost [:click 1]
                 :msg "store 3 [Credits]"
                 :once :per-turn
                 :effect (effect (add-counter card :credit 3))}
                {:cost [:click 1]
                 :msg (msg "gain " (get-in card [:counter :credit] 0) " [Credits]")
                 :once :per-turn
                 :label "Take all credits"
                 :effect (req (gain state side :credit (get-in card [:counter :credit] 0))
                              (add-counter state side card :credit (- (get-in card [:counter :credit] 0))))}]}})
