(in-ns 'game.cards.assets)

(def card-definition-allele-repression
  {"Allele Repression"
   {:implementation "Card swapping is manual"
    :advanceable :always
    :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                 :effect (effect (trash card {:cause :ability-cost}))
                 :msg (msg "swap " (get-counters card :advancement) " cards in HQ and Archives")}]}})
