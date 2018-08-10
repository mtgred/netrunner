(in-ns 'game.cards.resources)

(def card-definition-earthrise-hotel
  {"Earthrise Hotel"
   (let [ability {:msg "draw 2 cards"
                  :once :per-turn
                  :counter-cost [:power 1]
                  :req (req (:runner-phase-12 @state))
                  :effect (req (draw state :runner 2)
                               (when (zero? (get-counters (get-card state card) :power))
                                 (trash state :runner card {:unpreventable true})))}]
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                      (cons (get-in @state [:runner :identity])
                                                            (all-active-installed state :runner))))))}
    :data {:counter {:power  3}}
    :events {:runner-turn-begins ability}
    :abilities [ability]})})
