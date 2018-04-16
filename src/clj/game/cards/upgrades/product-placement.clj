(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-product-placement
  {"Product Placement"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain :corp :credit 2))}}})
