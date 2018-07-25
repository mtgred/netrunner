(in-ns 'game.cards.programs)

(def card-definition-tapwrm
  {"Tapwrm"
   (let [ability {:label "Gain [Credits] (start of turn)"
                  :msg (msg "gain " (quot (:credit corp) 5) " [Credits]")
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (gain-credits (quot (:credit corp) 5)))}]
     {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
      :flags {:drip-economy true}
      :abilities [ability]
      :events {:runner-turn-begins ability
               :purge {:effect (effect (trash card {:cause :purge}))}}})})
