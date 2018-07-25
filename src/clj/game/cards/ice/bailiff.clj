(in-ns 'game.cards.ice)

(def card-definition-bailiff
  {"Bailiff"
   {:implementation "Gain credit is manual"
    :abilities [(gain-credits-sub 1)]
    :subroutines [end-the-run]}})
