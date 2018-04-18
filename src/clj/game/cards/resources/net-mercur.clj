(in-ns 'game.core)

(def card-definitions-resources-net-mercur
  {"Net Mercur"
   {:abilities [{:counter-cost [:credit 1]
                 :msg "gain 1 [Credits]"
                 :effect (effect (gain :credit 1)
                                 (trigger-event :spent-stealth-credit card))}]
    :events {:spent-stealth-credit
             {:req (req (and (:run @state)
                             (has-subtype? target "Stealth")))
              :once :per-run
              :delayed-completion true
              :effect (effect (show-wait-prompt :corp "Runner to use Net Mercur")
                              (continue-ability
                                {:prompt "Place 1 [Credits] on Net Mercur or draw 1 card?"
                                 :player :runner
                                 :choices ["Place 1 [Credits]" "Draw 1 card"]
                                 :effect (req (if (= target "Draw 1 card")
                                                (do (draw state side)
                                                    (clear-wait-prompt state :corp)
                                                    (system-msg state :runner (str "uses Net Mercur to draw 1 card")))
                                                (do (add-counter state :runner card :credit 1)
                                                    (clear-wait-prompt state :corp)
                                                    (system-msg state :runner (str "places 1 [Credits] on Net Mercur")))))}
                               card nil))}}}})
