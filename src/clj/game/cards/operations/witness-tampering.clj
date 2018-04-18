(in-ns 'game.core)

(def card-definitions-operations-witness-tampering
  {"Witness Tampering"
   {:msg "remove 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
