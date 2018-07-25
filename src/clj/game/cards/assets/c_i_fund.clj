(in-ns 'game.cards.assets)

(def card-definition-c-i-fund
  {"C.I. Fund"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (pos? (:credit corp)))}
    :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                 :prompt "Choose how many [Credit] to move"
                 :once :per-turn
                 :choices {:number (req (min (:credit corp) 3))}
                 :effect (effect (lose-credits target)
                                 (add-counter card :credit target))
                 :msg (msg "move " target " [Credit] to C.I. Fund")}
                {:label "Take all credits from C.I. Fund"
                 :cost [:credit 2]
                 :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (take-credits (get-counters card :credit)))}]
    :events {:corp-turn-begins {:req (req (>= (get-counters card :credit) 6))
                                :effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credits] to C.I. Fund")))}}}})
