(in-ns 'game.cards.resources)

(def card-definition-kati-jones
  {"Kati Jones"
   {:abilities [{:cost [:click 1]
                 :msg "store 3 [Credits]"
                 :once :per-turn
                 :effect (effect (add-counter card :credit 3))}
                {:cost [:click 1]
                 :msg (msg "gain " (get-counters card :credit) " [Credits]")
                 :once :per-turn
                 :label "Take all credits"
                 :effect (req (gain-credits state side (get-counters card :credit))
                              (add-counter state side card :credit (- (get-counters card :credit))))}]}})
