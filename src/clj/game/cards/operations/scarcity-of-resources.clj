(in-ns 'game.core)

(def card-operations-scarcity-of-resources
  {"Scarcity of Resources"
   {:msg "increase the install cost of resources by 2"
    :events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 2]))}}}})