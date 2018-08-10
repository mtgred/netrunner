(in-ns 'game.cards.operations)

(def card-definition-scarcity-of-resources
  {"Scarcity of Resources"
   {:msg "increase the install cost of resources by 2"
    :events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 2]))}}}})
