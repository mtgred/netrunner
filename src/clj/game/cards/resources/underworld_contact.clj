(in-ns 'game.cards.resources)

(def card-definition-underworld-contact
  {"Underworld Contact"
   (let [ability {:label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (req (when (and (>= (:link runner) 2)
                                          (:runner-phase-12 @state))
                                 (system-msg state :runner (str "uses " (:title card) " to gain 1 [Credits]"))
                                 (gain-credits state :runner 1)))}]
   {:flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})})
