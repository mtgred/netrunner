(in-ns 'game.cards.resources)

(def card-definition-miss-bones
  {"Miss Bones"
   {:data {:counter {:credit 12}}
    :implementation "Credit use restriction not enforced"
    :abilities [{:counter-cost [:credit 1]
                 :msg "gain 1 [Credits] for trashing installed cards"
                 :async true
                 :effect (req (take-credits state :runner 1)
                              (if (zero? (get-counters (get-card state card) :credit))
                                (trash state :runner eid card {:unpreventable true})
                                (effect-completed state :runner eid)))}]}})
