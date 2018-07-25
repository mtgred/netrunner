(in-ns 'game.cards.operations)

(def card-definition-too-big-to-fail
  {"Too Big to Fail"
   {:req (req (< (:credit corp) 10))
    :msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain-credits 7)
                    (gain-bad-publicity :corp 1))}})
