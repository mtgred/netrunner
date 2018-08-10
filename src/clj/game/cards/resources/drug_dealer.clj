(in-ns 'game.cards.resources)

(def card-definition-drug-dealer
  {"Drug Dealer"
   {:flags {:runner-phase-12 (req (some #(card-flag? % :drip-economy true) (all-active-installed state :runner)))}
    :abilities [{:label "Lose 1 [Credits] (start of turn)"
                 :msg (msg (if (zero? (get-in @state [:runner :credit]))
                             "lose 0 [Credits] (runner has no credits to lose)"
                             "lose 1 [Credits]"))
                 :req (req (:runner-phase-12 @state))
                 :once :per-turn
                 :effect (effect (lose-credits 1))}]
    :events {:corp-turn-begins {:msg (msg "draw " (if (zero? (count (get-in @state [:runner :deck])))
                                                    "0 cards (runner's stack is empty)"
                                                    "1 card"))
                                :effect (effect (draw :runner 1))}
             :runner-turn-begins {:msg (msg "lose " (if (zero? (get-in @state [:runner :credit]))
                                                      "0 [Credits] (runner has no credits to lose)"
                                                      "1 [Credits]"))
                                  :once :per-turn
                                  :effect (effect (lose-credits 1))}}}})
