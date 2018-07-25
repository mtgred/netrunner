(in-ns 'game.cards.identities)

(def card-definition-iain-stirling-retired-spook
  {"Iain Stirling: Retired Spook"
   (let [ability {:req (req (> (:agenda-point corp) (:agenda-point runner)))
                  :once :per-turn
                  :msg "gain 2 [Credits]"
                  :effect (effect (gain-credits 2))}]
     {:flags {:drip-economy true}
      :events {:runner-turn-begins ability}
      :abilities [ability]})})
