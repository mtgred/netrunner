(in-ns 'game.cards.resources)

(def card-definition-armitage-codebusting
  {"Armitage Codebusting"
   {:data {:counter {:credit 12}}
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 2]
                 :msg "gain 2 [Credits]"
                 :effect (req (gain-credits state :runner 2)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (trash state :runner card {:unpreventable true})))}]}})
