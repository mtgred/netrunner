(in-ns 'game.cards.ice)

(def card-definition-fairchild-1-0
  {"Fairchild 1.0"
   {:subroutines [{:label "Force the Runner to pay 1 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 1 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 1 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 1 [Credits]")
                                  (do (pay state side card :credit 1)
                                      (system-msg state side "pays 1 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
