(in-ns 'game.core)

(def card-definitions-ice-fairchild-30
  {"Fairchild 3.0"
   {:subroutines [{:label "Force the Runner to pay 3 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 3 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 3 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 3 [Credits]")
                                  (do (pay state side card :credit 3)
                                      (system-msg state side "pays 3 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}
                  {:label "Do 1 brain damage or end the run"
                   :prompt "Choose one"
                   :choices ["Do 1 brain damage" "End the run"]
                   :msg (msg (lower-case target))
                   :effect (req (if (= target "Do 1 brain damage")
                                  (damage state side eid :brain 1 {:card card})
                                  (end-run state side)))}]
    :runner-abilities [(runner-break [:click 3] 3)]}})
