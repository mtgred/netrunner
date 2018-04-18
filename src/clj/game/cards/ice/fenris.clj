(in-ns 'game.core)

(def card-definitions-ice-fenris
  {"Fenris"
   {:effect take-bad-pub
    :subroutines [(do-brain-damage 1)
                  end-the-run]}})
