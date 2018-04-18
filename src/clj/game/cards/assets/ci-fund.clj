(in-ns 'game.core)

(def card-definitions-assets-ci-fund
  {"C.I. Fund"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (> (:credit corp) 0))}
    :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                 :prompt "Choose how many [Credit] to move" :once :per-turn
                 :choices {:number (req (min (:credit corp) 3))}
                 :effect (effect (lose :credit target)
                                 (add-counter card :credit target))
                 :msg (msg "move " target " [Credit] to C.I. Fund")}
                {:label "Take all credits from C.I. Fund"
                 :cost [:credit 2]
                 :msg (msg "trash it and gain " (get-in card [:counter :credit] 0) " [Credits]")
                 :effect (effect (gain :credit (get-in card [:counter :credit] 0))
                                 (trash card {:cause :ability-cost}))}]
    :events {:corp-turn-begins {:req (req (>= (get-in card [:counter :credit] 0) 6))
                                :effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credit] to C.I. Fund")))}}}})
