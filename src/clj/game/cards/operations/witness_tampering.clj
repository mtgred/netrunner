(in-ns 'game.cards.operations)

(def card-definition-witness-tampering
  {"Witness Tampering"
   {:msg "remove 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
