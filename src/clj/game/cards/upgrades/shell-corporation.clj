(in-ns 'game.core)

(def card-definitions-upgrades-shell-corporation
  {"Shell Corporation"
   {:abilities
    [{:cost [:click 1]
      :msg "store 3 [Credits]" :once :per-turn
      :effect (effect (add-counter card :credit 3))}
     {:cost [:click 1]
      :msg (msg "gain " (get-in card [:counter :credit] 0) " [Credits]") :once :per-turn
      :label "Take all credits"
      :effect (effect (gain :credit (get-in card [:counter :credit] 0))
                      (set-prop card :counter {:credit 0}))}]}})
