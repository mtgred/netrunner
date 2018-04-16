(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-drug-dealer
  {"Drug Dealer"
   {:flags {:runner-phase-12 (req (some #(card-flag? % :drip-economy true) (all-active-installed state :runner)))}
    :abilities [{:label "Lose 1 [Credits] (start of turn)"
                 :msg (msg (if (= (get-in @state [:runner :credit]) 0) "lose 0 [Credits] (runner has no credits to lose)" "lose 1 [Credits]"))
                 :req (req (:runner-phase-12 @state))
                 :once :per-turn
                 :effect (effect (lose :credit 1))}]
    :events {:corp-turn-begins {:msg (msg "draw " (if (= (count (get-in @state [:runner :deck])) 0)
                                                   "0 cards (runner's stack is empty)"
                                                   "1 card"))
                                :effect (effect (draw :runner 1))}
             :runner-turn-begins {:msg (msg "lose " (if (= (get-in @state [:runner :credit]) 0)
                                                             "0 [Credits] (runner has no credits to lose)"
                                                             "1 [Credits]"))
                                  :once :per-turn
                                  :effect (effect (lose :credit 1))}}}})
