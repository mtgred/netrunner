(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-iain-stirling-retired-spook
  {"Iain Stirling: Retired Spook"
   (let [ability {:req (req (> (:agenda-point corp) (:agenda-point runner)))
                  :once :per-turn
                  :msg "gain 2 [Credits]"
                  :effect (effect (gain :credit 2))}]
     {:flags {:drip-economy true}
      :events {:runner-turn-begins ability}
      :abilities [ability]})})
