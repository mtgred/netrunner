(in-ns 'game.cards.assets)

(def card-definition-expose
  {"Exposé"
   {:advanceable :always
    :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                 :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (lose :bad-publicity (get-counters card :advancement)))}]}})
