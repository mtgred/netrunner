(in-ns 'game.cards.ice)

(def card-definition-archer
  {"Archer"
   {:additional-cost [:forfeit]
    :subroutines [(gain-credits-sub 2)
                  trash-program
                  end-the-run]}})
