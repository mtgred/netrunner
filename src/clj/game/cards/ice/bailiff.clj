(in-ns 'game.core)

(def card-definitions-ice-bailiff
  {"Bailiff"
   {:implementation "Gain credit is manual"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]}})
