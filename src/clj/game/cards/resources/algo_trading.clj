(in-ns 'game.cards.resources)

(def card-definition-algo-trading
  {"Algo Trading"
   {:flags {:runner-phase-12 (req (pos? (:credit runner)))}
    :abilities [{:label "Move up to 3 [Credit] from credit pool to Algo Trading"
                 :prompt "Choose how many [Credit] to move" :once :per-turn
                 :choices {:number (req (min (:credit runner) 3))}
                 :effect (effect (lose-credits target)
                                 (add-counter card :credit target))
                 :msg (msg "move " target " [Credit] to Algo Trading")}
                {:label "Take all credits from Algo Trading"
                 :cost [:click 1]
                 :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                 :effect (effect (gain-credits (get-counters card :credit))
                                 (trash card {:cause :ability-cost}))}]
    :events {:runner-turn-begins {:req (req (>= (get-counters card :credit) 6))
                                  :effect (effect (add-counter card :credit 2)
                                                  (system-msg (str "adds 2 [Credit] to Algo Trading")))}}}})
