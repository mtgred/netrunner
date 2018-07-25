(in-ns 'game.cards.upgrades)

(def card-definition-product-placement
  {"Product Placement"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain-credits :corp 2))}}})
