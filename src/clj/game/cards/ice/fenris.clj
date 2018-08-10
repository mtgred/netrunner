(in-ns 'game.cards.ice)

(def card-definition-fenris
  {"Fenris"
   {:effect take-bad-pub
    :subroutines [(do-brain-damage 1)
                  end-the-run]}})
