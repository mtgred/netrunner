(in-ns 'game.core)

(def card-definitions-ice-archer
  {"Archer"
   {:additional-cost [:forfeit]
    :subroutines [(gain-credits 2)
                  trash-program
                  end-the-run]}})
