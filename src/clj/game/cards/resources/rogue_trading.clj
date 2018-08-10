(in-ns 'game.cards.resources)

(def card-definition-rogue-trading
  {"Rogue Trading"
   {:data {:counter {:credit 18}}
    :abilities [{:cost [:click 2]
                 :counter-cost [:credit 6]
                 :msg "gain 6 [Credits] and take 1 tag"
                 :effect (req (gain-credits state :runner 6)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (trash state :runner card {:unpreventable true}))
                              (gain-tags state :runner eid 1))}]}})
