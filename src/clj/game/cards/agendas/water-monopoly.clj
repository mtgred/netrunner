(in-ns 'game.core)

(def card-definitions-agendas-water-monopoly
  {"Water Monopoly"
   {:events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (has-subtype? target "Virtual"))
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 1]))}}}})
