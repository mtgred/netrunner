(in-ns 'game.core)

(def card-definitions-upgrades-product-placement
  {"Product Placement"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain :corp :credit 2))}}})
