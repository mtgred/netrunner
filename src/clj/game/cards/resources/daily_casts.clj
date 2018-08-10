(in-ns 'game.cards.resources)

(def card-definition-daily-casts
  {"Daily Casts"
   (let [ability {:once :per-turn
                  :label "Take 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"
                  :req (req (:runner-phase-12 @state))
                  :counter-cost [:credit 2]
                  :effect (req (gain-credits state :runner 2)
                               (when (zero? (get-counters (get-card state card) :credit))
                                 (trash state :runner card {:unpreventable true})))}]
   {:data {:counter {:credit 8}}
    :flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})})
