(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-earthrise-hotel
  {"Earthrise Hotel"
   (let [ability {:msg "draw 2 cards"
                  :once :per-turn
                  :counter-cost [:power 1]
                  :req (req (:runner-phase-12 @state))
                  :effect (req (draw state :runner 2)
                               (when (zero? (get-in card [:counter :power] 0))
                                 (trash state :runner card {:unpreventable true})))}]
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                      (cons (get-in @state [:runner :identity])
                                                            (all-active-installed state :runner))))))}
    :data {:counter {:power  3}}
    :events {:runner-turn-begins ability}
    :abilities [ability]})})
