(in-ns 'game.cards.operations)

(def card-definition-lateral-growth
  {"Lateral Growth"
   {:async true
    :msg "gain 4 [Credits]"
    :effect (effect (gain-credits 4)
                    (continue-ability {:player :corp
                                       :prompt "Select a card to install"
                                       :choices {:req #(and (= (:side %) "Corp")
                                                            (not (is-type? % "Operation"))
                                                            (in-hand? %))}
                                       :async true
                                       :msg (msg (corp-install-msg target))
                                       :effect (effect (corp-install eid target nil nil))}
                                      card nil))}})
