(in-ns 'game.core)

(def card-definitions-agendas-ar-enhanced-security
  {"AR-Enhanced Security"
   {:events {:runner-trash {:once :per-turn
                            :delayed-completion true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg "give the Runner a tag for trashing a Corp card"
                            :effect (effect (tag-runner :runner eid 1))}}}})
