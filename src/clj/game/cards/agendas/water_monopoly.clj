(in-ns 'game.cards.agendas)

(def card-definition-water-monopoly
  {"Water Monopoly"
   {:events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (has-subtype? target "Virtual"))
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 1]))}}}})
