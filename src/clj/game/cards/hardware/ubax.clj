(in-ns 'game.core)

(def card-hardware-ubax
  {"Ubax"
   (let [ability {:req (req (:runner-phase-12 @state))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1))}]
     {:in-play [:memory 1]
      :flags {:runner-turn-draw true
              :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                        (cons (get-in @state [:runner :identity])
                                                              (all-active-installed state :runner))))))}
      :events {:runner-turn-begins ability}
      :abilities [ability]})})
