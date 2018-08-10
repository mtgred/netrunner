(in-ns 'game.cards.resources)

(def card-definition-ghost-runner
  {"Ghost Runner"
   {:data {:counter {:credit 3}}
    :abilities [{:counter-cost [:credit 1]
                 :msg "gain 1 [Credits]"
                 :req (req (:run @state))
                 :effect (req (gain-credits state side 1)
                              (trigger-event state side :spent-stealth-credit card)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (trash state :runner card {:unpreventable true})))}]}})
