(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-ar-enhanced-security
  {"AR-Enhanced Security"
   {:events {:runner-trash {:once :per-turn
                            :delayed-completion true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg "give the Runner a tag for trashing a Corp card"
                            :effect (effect (tag-runner :runner eid 1))}}}})
