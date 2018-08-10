(in-ns 'game.cards.ice)

(def card-definition-fairchild-2-0
  {"Fairchild 2.0"
   {:subroutines [{:label "Force the Runner to pay 2 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 2 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 2 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 2 [Credits]")
                                  (do (pay state side card :credit 2)
                                      (system-msg state side "pays 2 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 2] 2)]}})
