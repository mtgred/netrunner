(in-ns 'game.core)

(def card-definitions-agendas-award-bait
  {"Award Bait"
   {:flags {:rd-reveal (req true)}
    :access {:delayed-completion true
             :req (req (not-empty (filter #(can-be-advanced? %) (all-installed state :corp))))
             :effect (effect (show-wait-prompt :runner "Corp to place advancement tokens with Award Bait")
                             (continue-ability
                               {:delayed-completion true
                                :choices ["0", "1", "2"]
                                :prompt "How many advancement tokens?"
                                :effect (req (let [c (str->int target)]
                                               (continue-ability
                                                 state side
                                                 {:choices {:req can-be-advanced?}
                                                  :msg (msg "place " c " advancement tokens on " (card-str state target))
                                                  :cancel-effect (req (clear-wait-prompt state :runner)
                                                                      (effect-completed state side eid))
                                                  :effect (effect (add-prop :corp target :advance-counter c {:placed true})
                                                                  (clear-wait-prompt :runner))} card nil)))}
                              card nil))}}})
