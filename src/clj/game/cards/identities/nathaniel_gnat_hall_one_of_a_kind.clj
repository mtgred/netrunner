(in-ns 'game.cards.identities)

(def card-definition-nathaniel-gnat-hall-one-of-a-kind
  {"Nathaniel \"Gnat\" Hall: One-of-a-Kind"
   (let [ability {:label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (req (when (and (> 3 (count (:hand runner)))
                                          (:runner-phase-12 @state))
                                 (system-msg state :runner (str "uses " (:title card) " to gain 1 [Credits]"))
                                 (gain-credits state :runner 1)))}]
     {:flags {:drip-economy true
              :runner-phase-12 (req (and (not (:disabled card))
                                         (some #(card-flag? % :runner-turn-draw true) (all-active-installed state :runner))))}
      :abilities [ability]
      :events {:runner-turn-begins ability}})})
