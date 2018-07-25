(in-ns 'game.cards.agendas)

(def card-definition-ar-enhanced-security
  {"AR-Enhanced Security"
   {:events {:runner-trash {:once :per-turn
                            :async true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg "give the Runner a tag for trashing a Corp card"
                            :effect (effect (gain-tags eid 1))}}}})
