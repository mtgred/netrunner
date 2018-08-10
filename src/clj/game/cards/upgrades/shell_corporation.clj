(in-ns 'game.cards.upgrades)

(def card-definition-shell-corporation
  {"Shell Corporation"
   {:abilities
    [{:cost [:click 1]
      :msg "store 3 [Credits]"
      :once :per-turn
      :effect (effect (add-counter card :credit 3))}
     {:cost [:click 1]
      :msg (msg "gain " (get-counters card :credit) " [Credits]")
      :once :per-turn
      :label "Take all credits"
      :effect (effect (take-credits (get-counters card :credit))
                      (set-prop card :counter {:credit 0}))}]}})
