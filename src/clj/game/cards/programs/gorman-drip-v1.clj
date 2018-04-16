(in-ns 'game.core)

(declare can-host?)

(def card-programs-gorman-drip-v1
  {"Gorman Drip v1"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit (get-virus-counters state side card))
                                                  (trash card {:cause :ability-cost}))
                 :msg (msg "gain " (get-virus-counters state side card) " [Credits]")}]
    :events {:corp-click-credit {:effect (effect (add-counter :runner card :virus 1))}
             :corp-click-draw {:effect (effect (add-counter :runner card :virus 1))}}}})