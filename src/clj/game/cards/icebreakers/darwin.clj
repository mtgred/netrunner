(in-ns 'game.cards.icebreakers)

(def card-definition-darwin
  {"Darwin"
   {:flags {:runner-phase-12 (req true)}
    :events {:purge {:effect (effect (update-breaker-strength card))}}
    :abilities [(break-sub 2 1 "ICE")
                {:label "Place 1 virus counter (start of turn)"
                 :once :per-turn
                 :cost [:credit 1]
                 :msg "place 1 virus counter"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (add-counter card :virus 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (or (get-virus-counters state side card) 0))}})
