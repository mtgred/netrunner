(in-ns 'game.core)

(def card-definitions-ice-himitsu-bako
  {"Himitsu-Bako"
   {:abilities [{:msg "add it to HQ"
                :cost [:credit 1]
                :effect (effect (move card :hand))}]
    :subroutines [end-the-run]}})
