(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-daily-casts
  {"Daily Casts"
   (let [ability {:once :per-turn
                  :label "Take 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"
                  :req (req (:runner-phase-12 @state))
                  :counter-cost [:credit 2]
                  :effect (req (gain state :runner :credit 2)
                               (when (zero? (get-in card [:counter :credit] 0))
                                 (trash state :runner card {:unpreventable true})))}]
   {:data {:counter {:credit 8}}
    :flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})})