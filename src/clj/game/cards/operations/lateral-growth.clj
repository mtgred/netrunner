(in-ns 'game.core)

(def card-operations-lateral-growth
  {"Lateral Growth"
   {:delayed-completion true
    :msg "gain 4 [Credits]"
    :effect (effect (gain :credit 4)
                    (continue-ability {:player :corp
                                       :prompt "Select a card to install"
                                       :choices {:req #(and (= (:side %) "Corp")
                                                            (not (is-type? % "Operation"))
                                                            (in-hand? %))}
                                       :delayed-completion true
                                       :msg (msg (corp-install-msg target))
                                       :effect (effect (corp-install eid target nil nil))}
                                      card nil))}})
