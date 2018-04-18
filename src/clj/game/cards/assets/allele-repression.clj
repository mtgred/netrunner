(in-ns 'game.core)

(def card-definitions-assets-allele-repression
  {"Allele Repression"
   {:implementation "Card swapping is manual"
    :advanceable :always
    :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                 :effect (effect (trash card))
                 :msg (msg "swap " (:advance-counter card 0) " cards in HQ and Archives")}]}})
