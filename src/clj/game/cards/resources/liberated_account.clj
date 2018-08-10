(in-ns 'game.cards.resources)

(def card-definition-liberated-account
  {"Liberated Account"
   {:data {:counter {:credit 16}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 4]
                 :msg "gain 4 [Credits]"
                 :effect (req (gain-credits state :runner 4)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (trash state :runner card {:unpreventable true})))}]}})
